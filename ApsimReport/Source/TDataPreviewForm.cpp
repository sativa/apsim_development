//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "TDataPreviewForm.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "AdvGrid"
#pragma link "BaseGrid"
#pragma link "DBAdvGrd"
#pragma resource "*.dfm"
TDataPreviewForm *DataPreviewForm;
//---------------------------------------------------------------------------
__fastcall TDataPreviewForm::TDataPreviewForm(TComponent* Owner)
   : TForm(Owner)
{
}
//---------------------------------------------------------------------------
