//---------------------------------------------------------------------------
#include <general\pch.h>
#include <vcl.h>
#pragma hdrstop

#include "TApsimFileReaderForm.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "AdvGrid"
#pragma link "BaseGrid"
#pragma link "dbadvgrd"
#pragma link "TPropertyForm"
#pragma link "DBAdvGrd"
#pragma link "AdvPanel"
#pragma resource "*.dfm"
TApsimFileReaderForm *ApsimFileReaderForm;
//---------------------------------------------------------------------------
__fastcall TApsimFileReaderForm::TApsimFileReaderForm(TComponent* Owner)
   : TPropertyForm(Owner)
{
}
//---------------------------------------------------------------------------
// Set the component which we are to edit.
//---------------------------------------------------------------------------
void TApsimFileReaderForm::setComponent(TComponent* comp)
   {
   TPropertyForm::setComponent(comp);

   apsimFileReader = dynamic_cast<TApsimFileReader*>(comp);
   FilesList->Clear();
   for (int f = 0; f != apsimFileReader->filenames->Count; f++)
      {
      TListItem* newItem = FilesList->Items->Add();
      newItem->Caption = apsimFileReader->filenames->Strings[f];
      }
   InterpretCheckBox->Checked = apsimFileReader->interpretTitles;
   FilesList->ViewStyle = vsReport;
   FilesList->ViewStyle = vsList;
   }
//---------------------------------------------------------------------------
// User has changed the interpret title checkbox.
//---------------------------------------------------------------------------
void __fastcall TApsimFileReaderForm::InterpretCheckBoxClick(
      TObject *Sender)
   {
   apsimFileReader->interpretTitles = InterpretCheckBox->Checked;
   }
//---------------------------------------------------------------------------
// User has clicked the edit button.
//---------------------------------------------------------------------------
void __fastcall TApsimFileReaderForm::Label1Click(TObject *Sender)
   {
   if (OpenDialog->Execute())
      {
      apsimFileReader->filenames = OpenDialog->Files;
      setComponent(apsimFileReader);
      }
   }
//---------------------------------------------------------------------------
void __fastcall TApsimFileReaderForm::Label4Click(TObject *Sender)
   {
   apsimFileReader->filenames->Clear();
   FilesList->Clear();
   }
//---------------------------------------------------------------------------

