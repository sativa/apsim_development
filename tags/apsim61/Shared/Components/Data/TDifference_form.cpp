//---------------------------------------------------------------------------
#include <general\pch.h>
#include <vcl.h>
#pragma hdrstop

#include "TDifference_form.h"
#include "TDifference_analysis.h"
#include <general\vcl_functions.h>
using namespace std;
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "TAnalysis_form"
#pragma link "paramtreeview"
#pragma resource "*.dfm"
TDifference_form *Difference_form;

//---------------------------------------------------------------------------
__fastcall TDifference_form::TDifference_form(TComponent* Owner)
   : TAnalysis_form(Owner)
{
}
//---------------------------------------------------------------------------
// Changes-
//    DAH 6/2/02: D-481 related. Changed getDataBlockNames to getAllDataBlockNames

void __fastcall TDifference_form::FormShow(TObject *Sender)
   {
   TAnalysis_form::FormShow(Sender);
   TDifference_analysis* Diff_analysis_ptr = dynamic_cast<TDifference_analysis*> (Analysis_ptr);
   if (Diff_analysis_ptr != NULL)
      {
      vector<string> pivotNames;
      Diff_analysis_ptr->sourceDataset->getAllDataBlockNames(pivotNames);
      Stl_2_tstrings(pivotNames, LHS1->Items);
      Stl_2_tstrings(pivotNames, LHS2->Items);
      Stl_2_tstrings(pivotNames, RHS1->Items);
      Stl_2_tstrings(pivotNames, RHS2->Items);

      if (Diff_analysis_ptr->LHS_pair->Count > 0)
         {
         LHS1->ItemIndex = LHS1->Items->IndexOf(Diff_analysis_ptr->LHS_pair->Strings[0]);
         RHS1->ItemIndex = RHS1->Items->IndexOf(Diff_analysis_ptr->RHS_pair->Strings[0]);
         }
      if (Diff_analysis_ptr->LHS_pair->Count > 1)
         {
         LHS2->ItemIndex = LHS2->Items->IndexOf(Diff_analysis_ptr->LHS_pair->Strings[1]);
         RHS2->ItemIndex = RHS2->Items->IndexOf(Diff_analysis_ptr->RHS_pair->Strings[1]);
         }
      }
   Combo_change(NULL);
   }
//---------------------------------------------------------------------------
void __fastcall TDifference_form::FormClose(TObject *Sender,
      TCloseAction &Action)
   {
   if (ModalResult == mrOk)
      {
      TAnalysis_form::FormClose(Sender, Action);
      TDifference_analysis* Diff_analysis_ptr = dynamic_cast<TDifference_analysis*> (Analysis_ptr);
      if (Diff_analysis_ptr != NULL)
         {
         Diff_analysis_ptr->LHS_pair->Clear();
         Diff_analysis_ptr->RHS_pair->Clear();
         if (LHS1->Text.Length() > 0)
            {
            Diff_analysis_ptr->LHS_pair->Add(LHS1->Text);
            Diff_analysis_ptr->RHS_pair->Add(RHS1->Text);
            }
         if (LHS2->Text.Length() > 0)
            {
            Diff_analysis_ptr->LHS_pair->Add(LHS2->Text);
            Diff_analysis_ptr->RHS_pair->Add(RHS2->Text);
            }
         }
      }
   }
//---------------------------------------------------------------------------
void __fastcall TDifference_form::Combo_change(TObject *Sender)
   {
   OkButton->Enabled = (LHS1->Text.Length() > 0 && RHS1->Text.Length() > 0);
   }
//---------------------------------------------------------------------------
bool TDifference_form::allowEnableOkButton(void)
   {
   fillCombos();
   return true;
   }
//---------------------------------------------------------------------------
void TDifference_form::fillCombos(void)
   {
   AnsiString savedLHS1 = LHS1->Text;
   AnsiString savedRHS1 = RHS1->Text;
   AnsiString savedLHS2 = LHS2->Text;
   AnsiString savedRHS2 = RHS2->Text;

   vector<string> dataBlockNames;
   Analysis_ptr->sourceDataset->getAllDataBlockNames(dataBlockNames);

   LHS1->Items->Clear();
   RHS1->Items->Clear();
   LHS2->Items->Clear();
   RHS2->Items->Clear();
   TTreeNode* node = ScenarioTree->Items->GetFirstNode();
   while (node != NULL)
      {
      if (node->Count == 0 && node->ImageIndex == 0)
         {
         AnsiString dataBlockName = dataBlockNames[(int)node->Data].c_str();
         LHS1->Items->Add(dataBlockName);
         RHS1->Items->Add(dataBlockName);
         LHS2->Items->Add(dataBlockName);
         RHS2->Items->Add(dataBlockName);
         }
      node = node->GetNext();
      }

   int indx = max(0, LHS1->Items->IndexOf(savedLHS1));
   LHS1->ItemIndex = indx;

   indx = RHS1->Items->IndexOf(savedRHS1);
   if (indx >= 0)
      RHS1->ItemIndex = indx;

   indx = LHS2->Items->IndexOf(savedLHS2);
   if (indx >= 0)
      LHS2->ItemIndex = indx;

   indx = RHS2->Items->IndexOf(savedRHS2);
   if (indx >= 0)
      RHS2->ItemIndex = indx;
   }

