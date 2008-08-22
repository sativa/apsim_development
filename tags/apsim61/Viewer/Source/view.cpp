//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "view.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "AdvGrid"
#pragma link "BaseGrid"
#pragma resource "*.dfm"
TViewForm *ViewForm;
//---------------------------------------------------------------------------
__fastcall TViewForm::TViewForm(TComponent* Owner)
   : TForm(Owner)
   {
   }
//---------------------------------------------------------------------------
void __fastcall TViewForm::RefreshView(void)
   {
   ViewFile(Caption);
   }
//---------------------------------------------------------------------------
void __fastcall TViewForm::ViewFile(String fileName)
   {
   Caption = fileName;

   TStringList *temp = new TStringList;
   temp->LoadFromFile(fileName);
   if(temp->Count < 2)return;
   StatusBar->Panels->Items[0]->Text = temp->Strings[0] + "  |  " + temp->Strings[1];
   temp->Delete(0);temp->Delete(0);

   String tmpFile = ChangeFileExt(fileName,".~mp");
   if(temp->Strings[0].Pos(","))outputGrid->Delimiter = ',';
   else
      {
      outputGrid->Delimiter = ' ';
      for(int i=0;i < temp->Count;i++)
         {
         for(int j=0;j < 10;j++)
            temp->Strings[i] = StringReplace(temp->Strings[i],"  "," ",
                                             TReplaceFlags() << rfReplaceAll);
         if(temp->Strings[i][1] == ' ')temp->Strings[i] = temp->Strings[i].Delete(1,1);
         }
      }
   temp->SaveToFile(tmpFile);
   outputGrid->Clear(); outputGrid->ColCount = 5;outputGrid->RowCount = 5;
   outputGrid->FixedCols = 1;

   outputGrid->LoadFromCSV(tmpFile);
   DeleteFile(tmpFile);

   delete temp;
   outputGrid->AutoSize = true;

   }
//---------------------------------------------------------------------------
void __fastcall TViewForm::outputGridClickCell(TObject *Sender, int ARow,int ACol)
   {
   if(ARow == 0)outputGrid->FixedCols = ACol + 1;
   }
//---------------------------------------------------------------------------
void __fastcall TViewForm::FormClose(TObject *Sender, TCloseAction &Action)
   {
   Action = caFree;
   }
//---------------------------------------------------------------------------



