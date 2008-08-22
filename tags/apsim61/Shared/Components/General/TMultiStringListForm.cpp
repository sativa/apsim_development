//---------------------------------------------------------------------------
#include <general\pch.h>
#include <vcl.h>
#pragma hdrstop

#include "TMultiStringListForm.h"
#pragma package(smart_init)
//---------------------------------------------------------------------------
#pragma resource "*.dfm"
TMultiStringListForm *MultiStringListForm;
// ------------------------------------------------------------------
//  Short description:           
//      constructor

//  Notes:

//  Changes:
//    DPH 5/2/98

// ------------------------------------------------------------------
__fastcall TMultiStringListForm::TMultiStringListForm(TComponent* Owner)
   : TForm(Owner)
   {
   }
// ------------------------------------------------------------------
//  Short description:
//      setup listbox OnFormShow

//  Notes:

//  Changes:
//    DPH 5/2/98

// ------------------------------------------------------------------
void __fastcall TMultiStringListForm::FormShow(TObject *Sender)
   {
   ListBox->Items->Clear();
   ListBox->Items->AddStrings(Multi_string_list_ptr->PossibleItems);
   for (int i = 0; i < Multi_string_list_ptr->Items->Count; i++)
      {
      int selected_index = ListBox->Items->IndexOf (Multi_string_list_ptr->Items->Strings[i]);
      ListBox->Selected[selected_index] = true;
      }
   }
// ------------------------------------------------------------------
//  Short description:
//      save listbox selections.

//  Notes:

//  Changes:
//    DPH 5/2/98

// ------------------------------------------------------------------
void __fastcall TMultiStringListForm::FormClose(TObject *Sender,
   TCloseAction &Action)
   {
   if (ModalResult == mrOk)
      {
      Multi_string_list_ptr->Items->Clear();
      for (int i = 0; i < ListBox->Items->Count; i++)
         {
         if (ListBox->Selected[i])
            Multi_string_list_ptr->Items->Add(ListBox->Items->Strings[i]);
         }
      }
   }
//---------------------------------------------------------------------------
void __fastcall TMultiStringListForm::ListBoxDblClick(TObject *Sender)
   {
   ModalResult = mrOk;
   }
//---------------------------------------------------------------------------

