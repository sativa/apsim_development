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
#pragma link "TSEGTableForm"
#pragma link "DBAdvGrd"
#pragma resource "*.dfm"
TApsimFileReaderForm *ApsimFileReaderForm;
//---------------------------------------------------------------------------
__fastcall TApsimFileReaderForm::TApsimFileReaderForm(TComponent* Owner)
   : TSEGTableForm(Owner)
{
}
//---------------------------------------------------------------------------
// Set the component which we are to edit.
//---------------------------------------------------------------------------
void TApsimFileReaderForm::setComponent(TApsimFileReader* apsimfilereader)
   {
   apsimFileReader = apsimfilereader;
   TSEGTableForm::setComponent(apsimFileReader);
   FilesList->Items->Assign(apsimFileReader->filenames);
   }
//---------------------------------------------------------------------------
// User has clicked the edit button.
//---------------------------------------------------------------------------
void __fastcall TApsimFileReaderForm::BrowseButtonClick(TObject *Sender)
   {
   if (OpenDialog->Execute())
      {
      FilesList->Items->Assign(OpenDialog->Files);
      apsimFileReader->filenames = FilesList->Items;
      FilesList->Items->Assign(apsimFileReader->filenames);
      }
   }
//---------------------------------------------------------------------------

