//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop

#include "TRotationForm.h"
#include "RotAddin.h"
using namespace std;

//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma resource "*.dfm"
TRotationForm *RotationForm;
//---------------------------------------------------------------------------
__fastcall TRotationForm::TRotationForm(TComponent* Owner)
   : TForm(Owner)
   {
   }
//---------------------------------------------------------------------------
void __fastcall TRotationForm::FormShow(TObject *Sender)
   {
   RotationCheck->Checked = rotationAddIn->getRotationAnalysisOn();
   }
//---------------------------------------------------------------------------
void __fastcall TRotationForm::FormClose(TObject *Sender,
      TCloseAction &Action)
   {
   if (ModalResult == mrOk)
      rotationAddIn->setRotationAnalysisOn(RotationCheck->Checked);
   }

