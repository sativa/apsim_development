//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "viewMain.h"
#include "view.h"
#include "About.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "MRUFList"
#pragma resource "*.dfm"
TmainForm *mainForm;
//---------------------------------------------------------------------------
__fastcall TmainForm::TmainForm(TComponent* Owner)
   : TForm(Owner)
   {
   }
//---------------------------------------------------------------------------
void __fastcall TmainForm::FileOpenAccept(TObject *Sender)
   {
   OpenFile(FileOpen->Dialog->FileName);
   }
//---------------------------------------------------------------------------
void __fastcall TmainForm::OpenFile(String fileName)
   {
   TViewForm *ViewForm = new TViewForm(Application);
   ViewForm->ViewFile(fileName);
   MRUFileList->AddItem(fileName);
   RefreshView->Enabled = MDIChildCount;
   FileSaveAsXL->Enabled = MDIChildCount;
   }
//---------------------------------------------------------------------------
void __fastcall TmainForm::MRUFileListMRUItemClick(TObject *Sender,
      AnsiString AFilename)
   {
   OpenFile(AFilename);
   }
//---------------------------------------------------------------------------
void __fastcall TmainForm::FormCreate(TObject *Sender)
   {
   DragAcceptFiles(Handle, true);
   }
//---------------------------------------------------------------------------
void __fastcall TmainForm::WMDropFiles(TWMDropFiles& Message)
   {
   // This code accepts files dropped onto a particular
   // Simplify with a shorter name for (HDROP)Message.Drop ...
   HDROP theDrop = (HDROP)Message.Drop;

   // Find out which node is indicated by the
   // cursor's position.
   POINT QueryPoint;
   DragQueryPoint(theDrop, &QueryPoint);

   // See on-line help "DragQueryFile()"
   // When called with 0xFFFFFFFF, returns number of files in HDROP.
   int fileCount = DragQueryFile(theDrop, 0xFFFFFFFF, NULL, 0);
   for ( int i = 0; i < fileCount; i++ )
      {
      // Call DragQueryFile() with 0 (zero) in 4th param
      // to get the size of the string it will return.
      int fileNameSize = DragQueryFile(theDrop, 0, NULL, 0);
      if ( fileNameSize != 0)
         {
         fileNameSize++;
         char *name = new char[fileNameSize];
         DragQueryFile(theDrop, i, name, fileNameSize);
         OpenFile(name);
         delete [] name;
         }
      }
      DragFinish(theDrop);
   }
//---------------------------------------------------------------------------
void __fastcall TmainForm::RefreshViewExecute(TObject *Sender)
   {
   TViewForm *ViewForm = (TViewForm *)ActiveMDIChild;
   ViewForm->RefreshView();
   }
//---------------------------------------------------------------------------
void __fastcall TmainForm::FormPaint(TObject *Sender)
   {
   RefreshView->Enabled = MDIChildCount;
   FileSaveAsXL->Enabled = MDIChildCount;
   }
//---------------------------------------------------------------------------
void __fastcall TmainForm::FormShow(TObject *Sender)
   {
   if(ParamCount() > 0)
         OpenFile(ParamStr(1));
   }
//---------------------------------------------------------------------------
void __fastcall TmainForm::FileSaveAsXLAccept(TObject *Sender)
   {
   // save current child mdi to an excel file
   TViewForm *ViewForm = (TViewForm *)ActiveMDIChild;
   String name = ExtractFileName(ViewForm->Caption);
   ViewForm->outputGrid->SaveToXLSSheet(FileSaveAsXL->Dialog->FileName,name);
   }
//---------------------------------------------------------------------------
void __fastcall TmainForm::About1Click(TObject *Sender)
   {
   AboutBox->ShowModal();   
   }
//---------------------------------------------------------------------------

