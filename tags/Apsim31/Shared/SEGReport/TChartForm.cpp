//---------------------------------------------------------------------------
#include <general\pch.h>
#include <vcl.h>
#pragma hdrstop

#include "TChartForm.h"
#include <general\vcl_functions.h>
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma resource "*.dfm"
TChartForm *ChartForm;
//---------------------------------------------------------------------------
__fastcall TChartForm::TChartForm(TComponent* Owner)
   : TForm(Owner)
{
}
//---------------------------------------------------------------------------
void TChartForm::setComponent(TSEGChart* c)
   {
   chart = c;
   NameEdit->Text = chart->Name;
   DuplicateCheckBox->Checked = chart->duplicateChartSeries;
   }
//---------------------------------------------------------------------------
void __fastcall TChartForm::NameEditExit(TObject *Sender)
   {
   chart->Name = NameEdit->Text;
   }
//---------------------------------------------------------------------------
void __fastcall TChartForm::ChartPropertiesButtonClick(TObject *Sender)
   {
   chart->userEdit();
   }
//---------------------------------------------------------------------------
void __fastcall TChartForm::DuplicateCheckBoxClick(TObject *Sender)
   {
   chart->duplicateChartSeries = DuplicateCheckBox->Checked;
   }
//---------------------------------------------------------------------------

