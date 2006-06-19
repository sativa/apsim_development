//---------------------------------------------------------------------------
#include <general\pch.h>
#include <vcl.h>
#pragma hdrstop
#include <general\string_functions.h>

#include "TChartLineForm.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "AdvGrid"
#pragma link "BaseGrid"
#pragma link "dbadvgrd"
#pragma link "TPropertyForm"
#pragma link "AdvPanel"
#pragma resource "*.dfm"
TChartLineForm *ChartLineForm;
//---------------------------------------------------------------------------
__fastcall TChartLineForm::TChartLineForm(TComponent* Owner)
   : TPropertyForm(Owner)
{
}
//---------------------------------------------------------------------------
// This is the component we're to modify.
//---------------------------------------------------------------------------
void TChartLineForm::setComponent(TComponent* component)
   {
   TPropertyForm::setComponent(component);

   chartLine = dynamic_cast<TChartLine*> (component);

   if (chartLine->source != NULL)
      {
      FieldNameCombo->Items->Assign(chartLine->source->FieldList);
      FieldNameCombo->Text = chartLine->fieldName;
      }
   X1Edit->Text = ftoa(chartLine->x1, 3).c_str();
   X2Edit->Text = ftoa(chartLine->x2, 3).c_str();
   LabelEdit->Text = chartLine->label;
   }
//---------------------------------------------------------------------------
void __fastcall TChartLineForm::FieldNameComboChange(TObject *Sender)
   {
   chartLine->fieldName = FieldNameCombo->Text;
   }
//---------------------------------------------------------------------------
void __fastcall TChartLineForm::X1EditChange(TObject *Sender)
   {
   chartLine->x1 = StrToFloat(X1Edit->Text);
   }
//---------------------------------------------------------------------------
void __fastcall TChartLineForm::X2EditChange(TObject *Sender)
   {
   chartLine->x2 = StrToFloat(X2Edit->Text);
   }
//---------------------------------------------------------------------------
void __fastcall TChartLineForm::LabelEditExit(TObject *Sender)
   {
   chartLine->label = LabelEdit->Text;
   }
//---------------------------------------------------------------------------

