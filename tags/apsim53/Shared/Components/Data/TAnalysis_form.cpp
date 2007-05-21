//---------------------------------------------------------------------------
#include <general\pch.h>
#include <vcl.h>
#pragma hdrstop

#include "TAnalysis_form.h"
#include "TAnalysis.h"
#include <general\vcl_functions.h>
#include <general\stristr.h>
#include <general\string_functions.h>
#include <assert.h>
using namespace std;
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "TAPSTable_form"
#pragma resource "*.dfm"
TAnalysis_form *Analysis_form;

static const char* YIELD_FIELD_NAME = "yield";
//---------------------------------------------------------------------------
__fastcall TAnalysis_form::TAnalysis_form(TComponent* Owner)
   : TAPSTable_form(Owner)
   {
   weAreExpanding = false;
   }
// ------------------------------------------------------------------
//  Short description:
//      FORM Show event handler - setup all scenarios and variables.

//  Changes:
//    DPH 3/8/2001
// ------------------------------------------------------------------
void __fastcall TAnalysis_form::FormShow(TObject *Sender)
   {
   if (Analysis_ptr != NULL)
      {
      showScenarios();
      showVariables();
      }
   }
// ------------------------------------------------------------------
//  Short description:
//      FORM Close event handler - save all scenarios and variables.

//  Changes:
//    DPH 3/8/2001
// ------------------------------------------------------------------
void __fastcall TAnalysis_form::FormClose(TObject *Sender,
      TCloseAction &Action)
   {
   if (ModalResult == mrOk && Analysis_ptr != NULL)
      {
      saveScenarios();
      saveVariables();
      }
   }
// ------------------------------------------------------------------
//  Short description:
//      Fill the scenarios GUI control with scenario names.

//  Changes:
//    DPH 3/8/2001
// ------------------------------------------------------------------
void TAnalysis_form::showScenarios(void)
   {
   ScenarioTree->Items->Clear();

   // Fill scenario select control
   vector<string> dataBlockNames;
   Analysis_ptr->sourceDataset->getAllDataBlockNames(dataBlockNames);
   for (vector<string>::iterator i = dataBlockNames.begin();
                                 i != dataBlockNames.end();
                                 i++)
      {
      vector<string> pivotNames;
      Split_string(*i, ";", pivotNames);

      // for each pivot name add to tree.  Each pivot name is nested down
      // one level from the previous.
      TTreeNode* node = NULL;
      for (vector<string>::iterator p = pivotNames.begin();
                                    p != pivotNames.end();
                                    p++)
         {
         AnsiString nodeName = (*p).c_str();
         TTreeNode* foundNode = findNodeInTree(node, nodeName);
         if (foundNode == NULL)
            {
            if (node == NULL)
               node = ScenarioTree->Items->Add(NULL, nodeName);
            else
               node = ScenarioTree->Items->AddChild(node, nodeName);

            if (Analysis_ptr->sourceDataset->isDataBlockVisible(i->c_str()))
               node->ImageIndex = 0;
            else
               node->ImageIndex = -1;
            node->Data = (void*) (i - dataBlockNames.begin());
            }
         else
            node = foundNode;
         }
      }
   expandAllNodes(true);
   }
// ------------------------------------------------------------------
//  Short description:
//      Fill the variable GUI control with variables.

//  Changes:
//    DPH 3/8/2001
// ------------------------------------------------------------------
void TAnalysis_form::showVariables(void)
   {
   VariableList->Items->Clear();

   TStringList* variables = Analysis_ptr->Field_names_to_analyse->PossibleItems;
   for (int i = 0; i < variables->Count; i++)
      {
      AnsiString name = variables->Strings[i];
      if (name != "Simulation")
         {
         TListItem* item = VariableList->Items->Add();
         item->Caption = name;
         if (Analysis_ptr->Field_names_to_analyse->Items->IndexOf(name) >= 0)
            item->Checked = true;
         }
      }
   if (Analysis_ptr->Field_names_to_analyse->Items->Count == 0)
      {
      bool oneFound = false;
      for (int i = 0; i < VariableList->Items->Count && !oneFound; i++)
         {
         if (stristr(VariableList->Items->Item[i]->Caption.c_str(), YIELD_FIELD_NAME) != NULL)
            {
            VariableList->Items->Item[i]->Checked = true;
            oneFound = true;
            }
         }
      }
   }
// ------------------------------------------------------------------
//  Short description:
//      Save all scenario selections from scenario control back
//      to the analysis table.

//  Changes:
//    DPH 3/8/2001
// ------------------------------------------------------------------
void TAnalysis_form::saveScenarios(void)
   {
   // If ModalResult == mrOk then loop through all nodes in tree
   // mark each data block name as either visible or invisible
   // depending on whether the user 'checked' them or not
   if (ModalResult == mrOk)
      {
      vector<string> dataBlockNames;
      Analysis_ptr->sourceDataset->getAllDataBlockNames(dataBlockNames);

      TTreeNode* node = ScenarioTree->Items->GetFirstNode();
      while (node != NULL)
         {
         if (node->Count == 0)
            Analysis_ptr->sourceDataset->markDataBlockVisible(dataBlockNames[(int)node->Data],
                                                (node->ImageIndex == 0));
         node = node->GetNext();
         }
      }
   }

// ------------------------------------------------------------------
//  Short description:
//      Save all variable selections from variable listbox back
//      to the analysis table.

//  Changes:
//    DPH 3/8/2001
// ------------------------------------------------------------------
void TAnalysis_form::saveVariables(void)
   {
   Analysis_ptr->Field_names_to_analyse->Items->Clear();

   // go find checked item.
   for (int i = 0; i < VariableList->Items->Count; i++)
      {
      if (VariableList->Items->Item[i]->Checked)
         Analysis_ptr->Field_names_to_analyse->Items->Add(VariableList->Items->Item[i]->Caption);
      }
   }
// ------------------------------------------------------------------
//  Short description:
//      Find a child with the specified text given the specified parent.
//      Returns NULL if not found.

//  Changes:
//    DPH 21/1/98
// ------------------------------------------------------------------
TTreeNode* TAnalysis_form::findNodeInTree (TTreeNode* ParentNode, AnsiString& Name)
   {
   if (ParentNode != NULL)
      {
      // loop through all children in tree and try and locate child.
      TTreeNode* ChildNode = ParentNode->getFirstChild();
      while (ChildNode != NULL)
         {
         if (ChildNode->Text == Name)
            return ChildNode;

         ChildNode = ParentNode->GetNextChild(ChildNode);
         }
      }
   else
      {
      // loop through all root nodes in tree and locate our node
      TTreeNode* RootNode = ScenarioTree->Items->GetFirstNode();
      while (RootNode != NULL)
         {
         if (RootNode->Text == Name)
            return RootNode;

         RootNode = RootNode->getNextSibling();
         }
      }

   return NULL;
   }
//---------------------------------------------------------------------------
void TAnalysis_form::expandAllNodes(bool expand)
   {
   if (expand)
      ScenarioTree->FullExpand();
   else
      ScenarioTree->FullCollapse();
      
/*
   The following code was not traversing the tree correctly - DAH
   TTreeNode* node = ScenarioTree->Items->GetFirstNode();
   while (node != NULL)
      {
      node->Expanded = expand;
      node = node->getNextSibling();
      }
*/
   }
//---------------------------------------------------------------------------
/*void __fastcall TAnalysis_form::ScenarioTreeChecked(TObject *Sender,
      TTreeNTNode *parent)
   {
   // fix a bug with TreeNT where the last node when checked becomes invisible.
   if (parent == ScenarioTree->Items->Item[ScenarioTree->Items->Count-1])
      parent->MakeVisible();

   // make all children same as parent wrt. checked checkboes.
   TTreeNTNode* child = parent->GetFirstChild();
   while (child != NULL)
      {
      child->CheckState = parent->CheckState;
      child = child->GetNextSibling();
      }

   checkOkButton();
   }
*///---------------------------------------------------------------------------
void __fastcall TAnalysis_form::VariableListChange(TObject *Sender,
      TListItem *Item, TItemChange Change)
   {
   static bool inVariableListChange = false;

   if (!inVariableListChange)
      {
      inVariableListChange = true;

      if (!multipleVariables && Item->Checked)
         {
         // make sure all other items are unchecked.

         for (int i = 0; i < VariableList->Items->Count; i++)
            {
            TListItem* thisItem = VariableList->Items->Item[i];
            if (thisItem->Caption != Item->Caption)
               thisItem->Checked = false;
            }
         }

      checkOkButton();
      inVariableListChange = false;
      }
   }
//---------------------------------------------------------------------------
void TAnalysis_form::checkOkButton(void)
   {
   // make sure at least one scenario is checked.
   bool enabled = false;
   TTreeNode* node = ScenarioTree->Items->GetFirstNode();
   while (node != NULL && !enabled)
      {
      while (node != NULL && node->Count > 0)
         node = node->GetNext();
      assert(node != NULL);

      enabled = (node->ImageIndex == 0);
      node = node->GetNext();
      }

   // make sure at least one variable is checked.
   if (enabled)
      {
      enabled = false;
      for (int i = 0; i < VariableList->Items->Count && !enabled; i++)
         enabled = VariableList->Items->Item[i]->Checked;
      }

   // if all still ok - then allow derived forms to disable it.
   enabled = (allowEnableOkButton() && enabled);

   OkButton->Enabled = enabled;
   }
//---------------------------------------------------------------------------
void __fastcall TAnalysis_form::ScenarioTreeClick(TObject *Sender)
   {
   if (weAreExpanding == false && ScenarioTree->Selected != NULL)
      {
      recursiveToggleImageOnNode(ScenarioTree->Selected);

      // make all children the same as the selected node.
      TTreeNode* node = ScenarioTree->Selected->getFirstChild();
      while (node != NULL)
         {
         node->ImageIndex = ScenarioTree->Selected->ImageIndex;
         node->SelectedIndex = node->ImageIndex;
         node = node->getNextSibling();
         }

      ScenarioTree->Refresh();
      checkOkButton();
      }
   weAreExpanding = false;
   }
//---------------------------------------------------------------------------
void TAnalysis_form::toggleImageOnNode(TTreeNode* node)
   {
   if (node->ImageIndex == -1)
      {
      node->ImageIndex = 0;
      node->SelectedIndex = 0;
      }
   else
      {
      node->ImageIndex = -1;
      node->SelectedIndex = -1;
      }
   }
//---------------------------------------------------------------------------
void TAnalysis_form::recursiveToggleImageOnNode(TTreeNode* node)
   {
   toggleImageOnNode(node);
   TTreeNode* next = node->getFirstChild();
   while (next != NULL)
      {
      recursiveToggleImageOnNode(next);
      next = next->getNextSibling();
      }
   }
//---------------------------------------------------------------------------



void __fastcall TAnalysis_form::ExpandLabelClick(TObject *Sender)
   {
   expandAllNodes(true);
   }
//---------------------------------------------------------------------------
void __fastcall TAnalysis_form::CollapseLabelClick(TObject *Sender)
   {
   expandAllNodes(false);
   }
//---------------------------------------------------------------------------
void __fastcall TAnalysis_form::ScenarioTreeExpanding(TObject *Sender,
      TTreeNode *Node, bool &AllowExpansion)
   {
   weAreExpanding = true;
   }
//---------------------------------------------------------------------------
void __fastcall TAnalysis_form::ScenarioTreeCollapsing(TObject *Sender,
      TTreeNode *Node, bool &AllowCollapse)
   {
   weAreExpanding = true;
   }
//---------------------------------------------------------------------------

