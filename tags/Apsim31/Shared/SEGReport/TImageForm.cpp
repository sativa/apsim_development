//---------------------------------------------------------------------------
#include <general\pch.h>
#include <vcl.h>
#pragma hdrstop

#include "TImageForm.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "AdvEdBtn"
#pragma link "AdvEdit"
#pragma resource "*.dfm"
TImageForm *ImageForm;
//---------------------------------------------------------------------------
__fastcall TImageForm::TImageForm(TComponent* Owner)
   : TForm(Owner)
{
}
//---------------------------------------------------------------------------
// The component passed in is what we're allowing the user to edit.
//---------------------------------------------------------------------------
void TImageForm::setComponent(TQRImage* i)
   {
   image = i;
   AutoSizeCheckBox->Checked = image->AutoSize;
   CentreCheckBox->Checked = image->Center;
   StretchCheckBox->Checked = image->Stretch;
   NameEdit->Text = image->Name;
   image->Zoom = 100;
   }
//---------------------------------------------------------------------------
void __fastcall TImageForm::AutoSizeCheckBoxClick(TObject *Sender)
   {
   image->AutoSize = AutoSizeCheckBox->Checked;
   }
//---------------------------------------------------------------------------
void __fastcall TImageForm::CentreCheckBoxClick(TObject *Sender)
   {
   image->Center = CentreCheckBox->Checked;
   }
//---------------------------------------------------------------------------
void __fastcall TImageForm::StretchCheckBoxClick(TObject *Sender)
   {
   image->Stretch = StretchCheckBox->Checked;
   }
//---------------------------------------------------------------------------
void __fastcall TImageForm::NameEditExit(TObject *Sender)
   {
   image->Name = NameEdit->Text;
   }
//---------------------------------------------------------------------------
void __fastcall TImageForm::AdvEditBtn1Click(TObject *Sender)
   {
   if (OpenPictureDialog->Execute())
      {
      image->Picture->LoadFromFile(OpenPictureDialog->FileName);
      ImageFileEdit->Text = OpenPictureDialog->FileName;
      }
   }
//---------------------------------------------------------------------------
