//---------------------------------------------------------------------------
#include <general\pch.h>
#include <vcl.h>
#pragma hdrstop

#include "TRotationForm.h"
#include "RotAddin.h"
#include "TRenameRotationForm.h"
using namespace std;

//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "dbslstbox"
#pragma link "slstbox"
#pragma resource "*.dfm"
TRotationForm *RotationForm;
//---------------------------------------------------------------------------
__fastcall TRotationForm::TRotationForm(TComponent* Owner)
   : TForm(Owner)
   {
   }
//---------------------------------------------------------------------------
void __fastcall TRotationForm::FormShow(TObject *Sender)
   {
   NumRotationsUpDown->Position = rotationAddIn->getNumRotations();

   vector<string> rotationNames;
   rotationAddIn->getRotationNames(rotationNames);
   ListBox->Sections->Clear();
   for (vector<string>::iterator rotationNameI = rotationNames.begin();
                                 rotationNameI != rotationNames.end();
                                 rotationNameI++)
      {
      TListSection* section = ListBox->Sections->Add();
      section->Caption = rotationNameI->c_str();

      vector<string> files;
      rotationAddIn->getRotation(*rotationNameI, files);
      for (vector<string>::iterator fileI = files.begin();
                                    fileI != files.end();
                                    fileI++)
         {
         section->SubItems->Add(fileI->c_str());
//         section->SubItemImageIdx[fileI - files.begin()] = 0;
         }
      }
   ListBox->ExpandAll();
   }
//---------------------------------------------------------------------------
void __fastcall TRotationForm::FormClose(TObject *Sender,
      TCloseAction &Action)
   {
   if (ModalResult == mrOk)
      {
      rotationAddIn->setRotationAnalysisOn(RotationCheck->Checked);
      rotationAddIn->clearRotations();
      for (int sectionI = 0; sectionI < ListBox->Sections->Count; sectionI++)
         {
         TListSection* section = ListBox->Sections->Items[sectionI];
         vector<string> filenames;
         for (int itemI = 0; itemI < ListBox->Sections->Items[sectionI]->SubItems->Count; itemI++)
            filenames.push_back(section->SubItems->Strings[itemI].c_str());
         rotationAddIn->addRotation(section->Caption.c_str(), filenames);
         }
      }
   }
//---------------------------------------------------------------------------
void __fastcall TRotationForm::ListBoxDragOver(TObject *Sender,
      TObject *Source, int x, int y, TDragState State, bool &Accept)
   {
   int sectionI, subItemI;
   Accept = ListBox->GetItemAtXY(x, y, sectionI, subItemI);
   }
//---------------------------------------------------------------------------
void __fastcall TRotationForm::ListBoxDragDrop(TObject *Sender,
      TObject *Source, int x, int y)
   {
   int dropSectionI, dropSubItemI;
   if (ListBox->GetItemAtXY(x, y, dropSectionI, dropSubItemI))
   if (dropSubItemI == -1)
      dropSubItemI = ListBox->Sections->Items[dropSectionI]->SubItems->Count;

   // Loop through all selected items and move to new location in list.
   // The GetListItemIndex method will throw when we pass it an invalid
   // itemI.  I can't figure out a better way to handle multiple selections.
   int itemI = 0;
   int sectionI, subItemI;
   try
      {
      while (true)
         {
         ListBox->GetListItemIndex(itemI, sectionI, subItemI);
         if (ListBox->Selected[itemI])
            moveItem(sectionI, subItemI, dropSectionI);
         else
            itemI++;
         }
      }
   catch (const Exception &E)
      {
      }
   ListBox->ExpandAll();
   }
//---------------------------------------------------------------------------
// move an item from one section to another.
//---------------------------------------------------------------------------
void TRotationForm::moveItem(int fromSectionID, int fromItemID,
                             int toSectionID)
   {
   AnsiString name = ListBox->Sections->Items[fromSectionID]->SubItems
                            ->Strings[fromItemID];
   ListBox->Sections->Items[fromSectionID]->SubItems->Delete(fromItemID);
   ListBox->Sections->Items[toSectionID]->SubItems->Append(name);
   }
//---------------------------------------------------------------------------
void __fastcall TRotationForm::NumRotationsEditChange(TObject *Sender)
   {
   int numSectionsNeeded = StrToInt(NumRotationsEdit->Text);
   // add additional sections if necessary.
   while (ListBox->Sections->Count < numSectionsNeeded)
      {
      TListSection* newSection = ListBox->Sections->Add();
      newSection->Caption = "Rotation" + IntToStr(ListBox->Sections->Count+1);
      }
   // remove sections from bottom if necessary.
   while (ListBox->Sections->Count > numSectionsNeeded)
      {
      int sectionToDeleteI = ListBox->Sections->Count-1;
      // move any items to first section.
      while (ListBox->Sections->Items[sectionToDeleteI]->SubItems->Count > 0)
         moveItem(sectionToDeleteI, 0, 0);
      ListBox->Sections->Delete(sectionToDeleteI);
      }
   }
//---------------------------------------------------------------------------
void __fastcall TRotationForm::RenameMenuItemClick(TObject *Sender)
   {
   unsigned sectionIndex = PopupMenu1->Tag;
   TListSection* section = ListBox->Sections->Items[sectionIndex];
   TRenameRotationForm* rotationRenameForm = new TRenameRotationForm(this);
   rotationRenameForm->RotationNameEdit->Text = section->Caption;
   if (rotationRenameForm->ShowModal() == mrOk)
      section->Caption = rotationRenameForm->RotationNameEdit->Text;
   delete rotationRenameForm;
   }
//---------------------------------------------------------------------------
void __fastcall TRotationForm::DeleteMenuItemClick(TObject *Sender)
   {
   if (Application->MessageBox("Are you sure you want to delete this rotation?",
                               "Question", MB_ICONQUESTION | MB_YESNO) == IDYES)
      {
      TListSection* sectionToDelete = ListBox->Sections->Items[PopupMenu1->Tag];

      // move all items from this section to an "unknown files" section.
      TListSection* unknownFilesSection = NULL;
      for (int sectionI = 0; sectionI < ListBox->Sections->Count; sectionI++)
         {
         if (ListBox->Sections->Items[sectionI]->Caption == "Unknown files")
            unknownFilesSection = ListBox->Sections->Items[sectionI];
         }
      if (unknownFilesSection == NULL)
         {
         unknownFilesSection = ListBox->Sections->Add();
         unknownFilesSection->Caption = "Unknown files";
         ListBox->ExpandAll();
         }
      unknownFilesSection->SubItems->AddStrings(sectionToDelete->SubItems);
      delete sectionToDelete;
      NumRotationsUpDown->Position--;
      }
   }
//---------------------------------------------------------------------------
void __fastcall TRotationForm::ListBoxMouseUp(TObject *Sender,
      TMouseButton Button, TShiftState Shift, int x, int y)
   {
   int sectionIndex, subItemIndex;
   if (Button == mbRight && ListBox->GetItemAtXY(x, y, sectionIndex, subItemIndex))
      {
      if (sectionIndex != -1 && subItemIndex == -1)
         {
         PopupMenu1->Tag = sectionIndex;
         DeleteMenuItem->Enabled = (ListBox->Sections->Items[sectionIndex]->Caption != "Unknown files");
         PopupMenu1->Popup(x + ListBox->ClientOrigin.x, y + ListBox->ClientOrigin.y);
         }
      }
   }
//---------------------------------------------------------------------------

