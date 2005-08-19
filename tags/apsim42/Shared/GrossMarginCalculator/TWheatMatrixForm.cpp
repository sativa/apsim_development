//---------------------------------------------------------------------------
#include <general\pch.h>
#include <vcl.h>
#pragma hdrstop

#include "TWheatMatrixForm.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "AdvCGrid"
#pragma link "AdvGrid"
#pragma link "BaseGrid"
#pragma resource "*.dfm"
TWheatMatrixForm *WheatMatrixForm;
//---------------------------------------------------------------------------
__fastcall TWheatMatrixForm::TWheatMatrixForm(TComponent* Owner)
   : TForm(Owner)
{
}
//---------------------------------------------------------------------------
