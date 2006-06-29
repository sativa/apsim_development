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
#pragma link "TPropertyForm"
#pragma link "DBAdvGrd"
#pragma link "AdvPanel"
#pragma resource "*.dfm"
TSOIForm *SOIForm;
//---------------------------------------------------------------------------
__fastcall TSOIForm::TSOIForm(TComponent* Owner)
   : TPropertyForm(Owner)
{
}
//---------------------------------------------------------------------------
// This is the component we're to modify.
//---------------------------------------------------------------------------
void TSOIForm::setComponent(TComponent* component)
   {
   TPropertyForm::setComponent(component);

   soi = dynamic_cast<TSOI*> (component);
   MonthCombo->ItemIndex = MonthCombo->Items->IndexOf(soi->month);
   NegativeCheckBox->Checked = soi->negative;
   PositiveCheckBox->Checked = soi->positive;
   FallingCheckBox->Checked = soi->falling;
   RisingCheckBox->Checked = soi->rising;
   ZeroCheckBox->Checked = soi->zero;
   GetFromSourceCheckBox->Checked = soi->GetSOIFromSource;
   AllOtherYearsCheckBox->Checked = soi->AllOtherYears;

   MonthCombo->Enabled = !GetFromSourceCheckBox->Checked;
   NegativeCheckBox->Enabled = !GetFromSourceCheckBox->Checked;
   PositiveCheckBox->Enabled = !GetFromSourceCheckBox->Checked;
   FallingCheckBox->Enabled = !GetFromSourceCheckBox->Checked;
   RisingCheckBox->Enabled = !GetFromSourceCheckBox->Checked;
   ZeroCheckBox->Enabled = !GetFromSourceCheckBox->Checked;
   }
//---------------------------------------------------------------------------
void __fastcall TSOIForm::MonthComboClick(TObject *Sender)
   {
   soi->month = MonthCombo->Text;
   }
//---------------------------------------------------------------------------
void __fastcall TSOIForm::NegativeCheckBoxClick(TObject *Sender)
   {
   soi->negative = NegativeCheckBox->Checked;
   }
//---------------------------------------------------------------------------
void __fastcall TSOIForm::PositiveCheckBoxClick(TObject *Sender)
   {
   soi->positive = PositiveCheckBox->Checked;
   }
//---------------------------------------------------------------------------
void __fastcall TSOIForm::FallingCheckBoxClick(TObject *Sender)
   {
   soi->falling = FallingCheckBox->Checked;
   }
//---------------------------------------------------------------------------
void __fastcall TSOIForm::RisingCheckBoxClick(TObject *Sender)
   {
   soi->rising = RisingCheckBox->Checked;
   }
//---------------------------------------------------------------------------
void __fastcall TSOIForm::ZeroCheckBoxClick(TObject *Sender)
   {
   soi->zero = ZeroCheckBox->Checked;
   }
//---------------------------------------------------------------------------
void __fastcall TSOIForm::GetFromSourceCheckBoxClick(TObject *Sender)
   {
   soi->GetSOIFromSource = GetFromSourceCheckBox->Checked;
   setComponent(soi);
   }
//---------------------------------------------------------------------------
void __fastcall TSOIForm::AllOtherYearsCheckBoxClick(TObject *Sender)
   {
   soi->AllOtherYears = AllOtherYearsCheckBox->Checked;
   }
//---------------------------------------------------------------------------

