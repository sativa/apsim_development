//---------------------------------------------------------------------------
#include <general\pch.h>
#include <vcl.h>
#pragma hdrstop

#include "TChartForm.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "TPropertyForm"
#pragma link "AdvPanel"
#pragma link "AdvCGrid"
#pragma link "AdvGrid"
#pragma link "AsgLinks"
#pragma link "BaseGrid"
#pragma resource "*.dfm"
TChartForm *ChartForm;
//---------------------------------------------------------------------------
__fastcall TChartForm::TChartForm(TComponent* Owner)
   : TPropertyForm(Owner)
{
}
//---------------------------------------------------------------------------
void TChartForm::setComponent(TComponent* component)
   {
   TPropertyForm::setComponent(component);
   graph = dynamic_cast<TGraph*>(component);
   SeriesNumberEdit->Text = graph->seriesNumbers;
   }
//---------------------------------------------------------------------------
void __fastcall TChartForm::SeriesNumberEditChange(TObject *Sender)
   {
   graph->seriesNumbers = SeriesNumberEdit->Text;
   }
//---------------------------------------------------------------------------
void __fastcall TChartForm::ChartPropertyLabelClick(TObject *Sender)
   {
   graph->userEdit();
   }
//---------------------------------------------------------------------------
void __fastcall TChartForm::FormShow(TObject *Sender)
   {
   // Setup
   }
//---------------------------------------------------------------------------

