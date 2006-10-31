//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "TStatsForm.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "AdvGrid"
#pragma link "BaseGrid"
#pragma resource "*.dfm"
TStatsForm *StatsForm;
//---------------------------------------------------------------------------
__fastcall TStatsForm::TStatsForm(TComponent* Owner)
   : TForm(Owner)
{
}
//---------------------------------------------------------------------------
