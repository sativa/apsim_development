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
   }
//---------------------------------------------------------------------------
void __fastcall TChartForm::ChartPropertyLabelClick(TObject *Sender)
   {
   graph->userEdit();
   }
//---------------------------------------------------------------------------

