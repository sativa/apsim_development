//---------------------------------------------------------------------------
#include <general\pch.h>
#include <vcl.h>
#pragma hdrstop

#include "TExcelForm.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "AdvGrid"
#pragma link "BaseGrid"
#pragma link "dbadvgrd"
#pragma link "TPropertyForm"
#pragma link "AdvEdBtn"
#pragma link "AdvEdit"
#pragma link "AdvFileNameEdit"
#pragma link "DBAdvGrd"
#pragma link "AdvPanel"
#pragma resource "*.dfm"
TExcelForm *ExcelForm;
//---------------------------------------------------------------------------
__fastcall TExcelForm::TExcelForm(TComponent* Owner)
   : TPropertyForm(Owner)
{
}
//---------------------------------------------------------------------------
// This is the component we're going to modify.
//---------------------------------------------------------------------------
void TExcelForm::setComponent(TComponent* component)
   {
   TPropertyForm::setComponent(component);

   excel = dynamic_cast<TExcel*>(component);
   FilenameEdit->Text = excel->fileName;
   PageCombo->Text = excel->pageName;
   }

//---------------------------------------------------------------------------
void __fastcall TExcelForm::FilenameEditChange(TObject *Sender)
   {
   excel->fileName = FilenameEdit->Text;
   PageCombo->Items->Assign(excel->pageNames);
   }
//---------------------------------------------------------------------------
void __fastcall TExcelForm::PageComboChange(TObject *Sender)
   {
   excel->pageName = PageCombo->Text;
   }
//---------------------------------------------------------------------------

