//---------------------------------------------------------------------------
#include <general\pch.h>
#include <vcl.h>
#pragma hdrstop

#include "TRecFilterForm.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "AdvGrid"
#pragma link "BaseGrid"
#pragma link "dbadvgrd"
#pragma link "TPropertyForm"
#pragma link "DBAdvGrd"
#pragma link "AdvPanel"
#pragma resource "*.dfm"
TRecFilterForm *RecFilterForm;
//---------------------------------------------------------------------------
__fastcall TRecFilterForm::TRecFilterForm(TComponent* Owner)
   : TPropertyForm(Owner)
   {
   }
//---------------------------------------------------------------------------
// This is the component we're to modify.
//---------------------------------------------------------------------------
void TRecFilterForm::setComponent(TComponent* component)
   {
   TPropertyForm::setComponent(component);

   recFilter = dynamic_cast<TRecFilter*>(component);
   FirstCheckBox->Checked = recFilter->firstRecord;
   LastCheckBox->Checked = recFilter->lastRecord;
   RecEdit->Text = IntToStr(recFilter->recordNumber);
   }
//---------------------------------------------------------------------------
void __fastcall TRecFilterForm::RecEditExit(TObject *Sender)
   {
   recFilter->recordNumber = StrToInt(RecEdit->Text);
   }
//---------------------------------------------------------------------------
void __fastcall TRecFilterForm::FirstCheckBoxClick(TObject *Sender)
   {
   recFilter->firstRecord = FirstCheckBox->Checked;
   }
//---------------------------------------------------------------------------
void __fastcall TRecFilterForm::LastCheckBoxClick(TObject *Sender)
   {
   recFilter->lastRecord = LastCheckBox->Checked;
   }
//---------------------------------------------------------------------------

