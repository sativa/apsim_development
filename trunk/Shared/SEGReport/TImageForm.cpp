//---------------------------------------------------------------------------
#include <general\pch.h>
#include <vcl.h>
#pragma hdrstop

#include "TImageForm.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "AdvEdBtn"
#pragma link "AdvEdit"
#pragma link "TPropertyForm"
#pragma link "AdvPanel"
#pragma resource "*.dfm"
TImageForm *ImageForm;
//---------------------------------------------------------------------------
__fastcall TImageForm::TImageForm(TComponent* Owner)
   : TPropertyForm(Owner)
{
}
//---------------------------------------------------------------------------
// The component passed in is what we're allowing the user to edit.
//---------------------------------------------------------------------------
void TImageForm::setComponent(TComponent* component)
   {
   TPropertyForm::setComponent(component);
   image = dynamic_cast< ::TImage*>(component);

   AutoSizeCheckBox->Checked = image->AutoSize;
   CentreCheckBox->Checked = image->Center;
   StretchCheckBox->Checked = image->Stretch;
   NameEdit->Text = image->Name;
   image->Zoom = image->ParentReport->Zoom;
   }
//---------------------------------------------------------------------------
void __fastcall TImageForm::AutoSizeCheckBoxClick(TObject *Sender)
   {
   if (image->AutoSize != AutoSizeCheckBox->Checked)
      image->AutoSize = AutoSizeCheckBox->Checked;
   }
//---------------------------------------------------------------------------
void __fastcall TImageForm::CentreCheckBoxClick(TObject *Sender)
   {
   if (image->Center != CentreCheckBox->Checked)
      image->Center = CentreCheckBox->Checked;
   }
//---------------------------------------------------------------------------
void __fastcall TImageForm::StretchCheckBoxClick(TObject *Sender)
   {
   if (image->Stretch != StretchCheckBox->Checked)
      image->Stretch = StretchCheckBox->Checked;
   }
//---------------------------------------------------------------------------
void __fastcall TImageForm::ImageFileEditClickBtn(TObject *Sender)
   {
   if (OpenPictureDialog->Execute())
      {
      image->Picture->LoadFromFile(OpenPictureDialog->FileName);
      ImageFileEdit->Text = OpenPictureDialog->FileName;
      }
   }
//---------------------------------------------------------------------------

