//---------------------------------------------------------------------------
#include <general\pch.h>
#include <vcl.h>
#pragma hdrstop

#include "TValueSelectPopup.h"
#include <ApsimShared\ApsimSettings.h>
#include <ApsimShared\ApsimDirectories.h>
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "POPUPFORMUNIT"
#pragma link "paramchklist"
#pragma resource "*.dfm"
TValueSelectPopup *ValueSelectPopup;
//---------------------------------------------------------------------------
__fastcall TValueSelectPopup::TValueSelectPopup(TComponent* Owner)
   : TPopupForm(Owner)
{
}
//---------------------------------------------------------------------------

void __fastcall TValueSelectPopup::ListViewCompare(TObject *Sender,
      TListItem *Item1, TListItem *Item2, int Data, int &Compare)
{
   if (ListView->SortType == Listactns::stData)  // would use the Data parameter, but the SortType
                                      // is not working as specced in helpfile
   {
      float i1, i2;
      char* endptr;
      i1 = strtod(Item1->Caption.c_str(), &endptr);
      i2 = strtod(Item2->Caption.c_str(), &endptr);
      if (i1 < i2)
         Compare = -1;
      else if (i1 > i2)
         Compare = 1;
      else
         Compare = 0;
   }

}
//---------------------------------------------------------------------------

void __fastcall TValueSelectPopup::FormClose(TObject *Sender,
      TCloseAction &Action)
{
   if (applied || appliedToAll)
      {
      SelectedItems.erase(SelectedItems.begin(), SelectedItems.end());
      for (int i = 0; i < ListView->Items->Count; i++)
         {
         if (ListView->Items->Item[i]->Checked)
            SelectedItems.push_back (ListView->Items->Item[i]->Caption.c_str());
         }
      }
}
//---------------------------------------------------------------------------

void __fastcall TValueSelectPopup::FormShow(TObject *Sender)
{
   applied = appliedToAll = false;
   ListView->Items->Clear();
   // work out what type of sorting is required for this listview
   ListView->SortType = WhatSortType(SelectedItems);
   for (vector<string>::iterator i = SelectedItems.begin();
                                 i != SelectedItems.end();
                                 i++)
      {
      TListItem* NewItem = ListView->Items->Add();
      NewItem->Caption = (*i).c_str();
      NewItem->Checked = ((*i) == CurrentValue);
      }
   if (ListView->SortType == Listactns::stData)
      ListView->AlphaSort();
   ListView->UpdateItems (0, ListView->Items->Count);

   // load the picture - if any.
   string fileName;
   ApsimSettings settings;
   settings.read("Bitmaps|" + factorName, fileName);
   if (fileName != "")
      fileName = getAppHomeDirectory() + "\\" + fileName;
   if (fileName == "" || !FileExists(fileName.c_str()))
      Image->Picture->Assign(&TPicture());
   else
      Image->Picture->LoadFromFile(fileName.c_str());
}
//---------------------------------------------------------------------------

TSortType TValueSelectPopup::WhatSortType(vector<string>& items)
{
   char *endptr;

   bool numerical = true;
   for (int i = 0; i < items.size(); i++)
      {
      strtod(items[i].c_str(), &endptr);
      bool thisItemIsNumerical = (*endptr == '\0' || *endptr == ' ');
      numerical = numerical && thisItemIsNumerical;
      }
   if (numerical)
      return Listactns::stData;
   else
      return Listactns::stNone;
}



void __fastcall TValueSelectPopup::applyLabelClick(TObject *Sender)
{
   applied = true;
   appliedToAll = false;
   Close();
}
//---------------------------------------------------------------------------

void __fastcall TValueSelectPopup::applyToAllLabelClick(TObject *Sender)
{
   appliedToAll = true;
   applied = false;
   Close();
}
//---------------------------------------------------------------------------



void __fastcall TValueSelectPopup::ListViewChange(TObject *Sender,
      TListItem *Item, TItemChange Change)
{
   if (checkedCount() > 0)
   {
      applyLabel->Enabled = true;
      applyToAllLabel->Enabled = true;
   }
   else
   {
      applyLabel->Enabled = false;
      applyToAllLabel->Enabled = false;
   }
}
//---------------------------------------------------------------------------

int TValueSelectPopup::checkedCount(void)
{
   int count = 0;
   for (int i = 0; i < ListView->Items->Count; i++)
   {
      if (ListView->Items->Item[i]->Checked)
         count++;
   }
   return count;
}
