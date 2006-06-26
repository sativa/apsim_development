//---------------------------------------------------------------------------
#include <general\pch.h>
#include <vcl.h>
#pragma hdrstop

#include "TFrequencyForm.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "BaseGrid"
#pragma link "TPropertyForm"
#pragma link "AdvPanel"
#pragma resource "*.dfm"
TFrequencyForm *FrequencyForm;
//---------------------------------------------------------------------------
__fastcall TFrequencyForm::TFrequencyForm(TComponent* Owner)
   : TPropertyForm(Owner)
{
}
//---------------------------------------------------------------------------
// This is the component we're to modify.
//---------------------------------------------------------------------------
void TFrequencyForm::setComponent(TComponent* component)
   {
   TPropertyForm::setComponent(component);

   frequency = dynamic_cast<TFrequency*> (component);

   Grid->Cells[0][0] = "Label";
   Grid->Cells[1][0] = "Filter";

   for (int i = 0; i != frequency->labels->Count; i++)
      {
      Grid->Cells[0][i+1] = frequency->labels->Strings[i];
      Grid->Cells[1][i+1] = frequency->filters->Strings[i];
      }
   }
//---------------------------------------------------------------------------
void __fastcall TFrequencyForm::GridEditingDone(TObject *Sender)
   {
   TStringList* Labels = new TStringList;
   TStringList* Filters = new TStringList;
   for (int row = 1; row != Grid->RowCount; row++)
      {
      if (Grid->Cells[0][row] != "")
         {
         Labels->Add(Grid->Cells[0][row]);
         Filters->Add(Grid->Cells[1][row]);
         }
      }
   frequency->labels = Labels;
   frequency->filters = Filters;
   delete Labels;
   delete Filters;
   }
//---------------------------------------------------------------------------

