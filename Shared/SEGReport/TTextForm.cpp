#include <general\pch.h>
#include <vcl.h>
#pragma hdrstop

#include "TTextForm.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma resource "*.dfm"
TTextForm *TextForm;
//---------------------------------------------------------------------------
__fastcall TTextForm::TTextForm(TComponent* Owner)
   : TForm(Owner)
{
}
//---------------------------------------------------------------------------
void TTextForm::setComponent(TText* richtext)
   {
   richText = richtext;
   TextEdit->Lines->Text = richText->text;
   NameEdit->Text = richText->Name;
   }
//---------------------------------------------------------------------------
void __fastcall TTextForm::TextEditExit(TObject *Sender)
   {
   richText->text = TextEdit->Lines->Text;
   }
//---------------------------------------------------------------------------
void __fastcall TTextForm::FontButtonClick(TObject *Sender)
   {
   FontDialog->Font = richText->Font;
   if (FontDialog->Execute())
      richText->Font->Assign(FontDialog->Font);
   }
//---------------------------------------------------------------------------
void __fastcall TTextForm::NameEditExit(TObject *Sender)
   {
   richText->Name = NameEdit->Text;
   }
//---------------------------------------------------------------------------

