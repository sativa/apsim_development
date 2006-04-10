//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop

#include "TSmallMultiListForm.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma resource "*.dfm"
TSmallMultiListForm *SmallMultiListForm;
//---------------------------------------------------------------------------
__fastcall TSmallMultiListForm::TSmallMultiListForm(TComponent* Owner)
   : TForm(Owner)
   {
   }
//---------------------------------------------------------------------------
void TSmallMultiListForm::Set_items (TStrings* Items)
   {
   ListBox1->Items->AddStrings(Items);
   }
//---------------------------------------------------------------------------
void TSmallMultiListForm::Set_selected_items (TStrings* Items_to_select)
   {
   for (int i = 0; i < ListBox1->Items->Count; i++)
      {
      int selected_index = ListBox1->Items->IndexOf (Items_to_select->Strings[i]);
      ListBox1->Selected[selected_index] = true;
      }
   }
//---------------------------------------------------------------------------
void TSmallMultiListForm::Get_selected_items (TStrings* Selected_items)
   {
   Selected_items->Clear();
   for (int i = 0; i < ListBox1->Items->Count; i++)
      {
      if (ListBox1->Selected[i])
         Selected_items->Add(ListBox1->Items->Strings[i]);
      }
   }
//---------------------------------------------------------------------------
void __fastcall TSmallMultiListForm::BitBtn1Click(TObject *Sender)
   {
   Close();
   }
//---------------------------------------------------------------------------

