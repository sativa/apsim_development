//---------------------------------------------------------------------------
#include <general\pch.h>
#include <vcl.h>
#pragma hdrstop

#include "TXmlFileReaderForm.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "AdvGrid"
#pragma link "BaseGrid"
#pragma link "dbadvgrd"
#pragma link "TPropertyForm"
#pragma link "DBAdvGrd"
#pragma link "AdvPanel"
#pragma resource "*.dfm"
TXmlFileReaderForm *XmlFileReaderForm;
//---------------------------------------------------------------------------
__fastcall TXmlFileReaderForm::TXmlFileReaderForm(TComponent* Owner)
   : TPropertyForm(Owner)
{
}
//---------------------------------------------------------------------------
// Set the component which we are to edit.
//---------------------------------------------------------------------------
void TXmlFileReaderForm::setComponent(TComponent* comp)
   {
   TPropertyForm::setComponent(comp);

   xmlFileReader = dynamic_cast<TXmlFileReader*>(comp);
   FileNameEdit->Text = xmlFileReader->filename;
   }
//---------------------------------------------------------------------------
// User has clicked the browse button.
//---------------------------------------------------------------------------
void __fastcall TXmlFileReaderForm::Label1Click(TObject *Sender)
   {
   if (OpenDialog->Execute())
      {
      xmlFileReader->filename = OpenDialog->FileName;
      setComponent(xmlFileReader);
      }
   }
//---------------------------------------------------------------------------

