//---------------------------------------------------------------------------
#include <general\pch.h>
#include <vcl.h>
#pragma hdrstop

#include "TMoveParametersForm.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "AdvEdBtn"
#pragma link "AdvEdit"
#pragma link "AdvFileNameEdit"
#pragma resource "*.dfm"
TMoveParametersForm *MoveParametersForm;
//---------------------------------------------------------------------------
__fastcall TMoveParametersForm::TMoveParametersForm(TComponent* Owner)
   : TForm(Owner)
   {
   }
//---------------------------------------------------------------------------

