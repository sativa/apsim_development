//---------------------------------------------------------------------------
#include <general\pch.h>
#include <vcl.h>
#pragma hdrstop

#include "TREMSForm.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "AdvGrid"
#pragma link "BaseGrid"
#pragma link "dbadvgrd"
#pragma link "TPropertyForm"
#pragma link "AdvEdBtn"
#pragma link "AdvEdit"
#pragma link "AdvFileNameEdit"
#pragma link "AdvPanel"
#pragma resource "*.dfm"
TREMSForm *REMSForm;
//---------------------------------------------------------------------------
__fastcall TREMSForm::TREMSForm(TComponent* Owner)
   : TPropertyForm(Owner)
{
}
//---------------------------------------------------------------------------
// This is the component we're going to modify.
//---------------------------------------------------------------------------
void TREMSForm::setComponent(TComponent* component)
   {
   TPropertyForm::setComponent(component);
   rems = dynamic_cast<TREMS*>(component);

   FilenameEdit->Text = rems->filename;
   ExperimentCombo->Text = rems->experiment;
   TreatmentCombo->Text = rems->treatment;
   DataSourceCombo->Text = rems->datasource;
   ExperimentCombo->Items->Assign(rems->experiments);
   TreatmentCombo->Items->Assign(rems->treatments);
   DataSourceCombo->Text = rems->datasource;
   }
//---------------------------------------------------------------------------
void __fastcall TREMSForm::FilenameEditChange(TObject *Sender)
   {
   rems->filename = FilenameEdit->Text;
   ExperimentCombo->Items->Assign(rems->experiments);
   }
//---------------------------------------------------------------------------
void __fastcall TREMSForm::ExperimentComboChange(TObject *Sender)
   {
   rems->experiment = ExperimentCombo->Text;
   TreatmentCombo->Items->Assign(rems->treatments);
   }
//---------------------------------------------------------------------------
void __fastcall TREMSForm::TreatmentComboChange(TObject *Sender)
   {
   rems->treatment = TreatmentCombo->Text;
   }
//---------------------------------------------------------------------------
void __fastcall TREMSForm::DataSourceComboChange(TObject *Sender)
   {
   rems->datasource = DataSourceCombo->Text;
   }
//---------------------------------------------------------------------------

