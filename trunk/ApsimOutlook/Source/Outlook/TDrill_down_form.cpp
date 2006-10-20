//---------------------------------------------------------------------------
#include <general\pch.h>
#include <vcl.h>
#pragma hdrstop

#include "TDrill_down_form.h"
#include "TValueSelectPopup.h"
#include "TScenarioSelectForm.h"

#include <general\vcl_functions.h>
#include <general\math_functions.h>
#include <general\string_functions.h>
#include <general\stl_functions.h>
#include <general\inifile.h>
#include <general\path.h>
#include <ApsimShared\ApsimSettings.h>
#include <ApsimShared\ApsimDirectories.h>

#include <components\general\tmultistringlist.h>
#include <strstream>
//---------------------------------------------------------------------------
#pragma link "Grids"
#pragma link "paramtreeview"
#pragma resource "*.dfm"

using namespace std;

TDrill_down_form *Drill_down_form;

static const char* BITMAPS_SECTION = "bitmaps";

// ------------------------------------------------------------------
//  Short description:
//    constructor

//  Notes:

//  Changes:
//    DPH 13/1/98

// ------------------------------------------------------------------
__fastcall TDrill_down_form::TDrill_down_form(TComponent* Owner)
   : TForm(Owner)
   {
   weAreExpanding = false;
   fullExpColl = false;
   renamingNode = false;
   ValueSelectPopup = NULL;
   }

// ------------------------------------------------------------------
//  Short description:
//    create all tabs.

//  Notes:

//  Changes:
//    DPH 29/6/98

// ------------------------------------------------------------------
void TDrill_down_form::refreshScenarioTree (void)
   {
   //capture important state information about the present tree
   //select a root node.
   int previous = -1;
   vector<int> expandedNodes;
   if (ScenarioTree->Items->Count > 0)
      {
      if (ScenarioTree->Selected != NULL && ScenarioTree->Selected->Level > 0)
         ScenarioTree->Selected->Parent->Selected = true;
      int nodeNum = 0;
      TTreeNode* node = ScenarioTree->Items->Item[0];
      while (node != NULL)
         {
         if (node->Expanded)
            expandedNodes.push_back(nodeNum);
         if (node->Selected)
            previous = nodeNum;
         nodeNum++;
         node = node->getNextSibling();
         }
      }

   ScenarioTree->Items->Clear();
   vector<string> Names;
   scenarios->getScenarioNames(Names);
   for (vector<string>::iterator i = Names.begin(); i!=Names.end(); i++)
      {
      AnsiString nameString = i->c_str();
      TTreeNode* newScen = ScenarioTree->Items->Add(NULL, nameString);
      vector<string> factorNames;
      scenarios->getFactorNames(*i, factorNames);
      for (vector<string>::iterator j = factorNames.begin(); j!=factorNames.end(); j++)
         {
         string valueString = *j + " = " + scenarios->getFactorValue(*i, *j);
         ScenarioTree->Items->AddChild(newScen, valueString.c_str());
         }
      }

   // highlight the logically 'current' scenario; ie. the first scenario closest
   // to the node previously highlighted
   int nodeNum = 0;
   TTreeNode* node = ScenarioTree->Items->Item[0];
   while (node != NULL)
      {
      if (find(expandedNodes.begin(), expandedNodes.end(), nodeNum) != expandedNodes.end())
         node->Expanded = true;
      if (nodeNum == previous)
         node->Selected = true;
      nodeNum++;
      node = node->getNextSibling();
      }

   // if there is only 1 root node in tree then expand it.
   if (ScenarioTree->Items->Item[0]->getNextSibling() == NULL)
      {
      ScenarioTree->Items->Item[0]->Expanded = true;
      weAreExpanding = false;
      }
   }

// ------------------------------------------------------------------
//  Short description:
//    refresh everything

//  Notes:

//  Changes:
//    DPH 29/6/98
//    DPH 1/6/99 changed to a scroll box instead of a grid.

// ------------------------------------------------------------------
void TDrill_down_form::Refresh (void)
   {
   fullExpColl = true;
   refreshScenarioTree();
   }
// ------------------------------------------------------------------
//  Short description:
//    form about to be shown.  initialise everything.

//  Notes:

//  Changes:
//    DPH 29/6/98

// ------------------------------------------------------------------
void __fastcall TDrill_down_form::FormShow(TObject *Sender)
   {
   scenarios->restore("Default");
   Refresh();

   string addInCaption = scenarios->getUIButtonCaption();
   if (addInCaption == "")
      {
      AddInBevel->Visible = false;
      AddInLabel->Visible = false;
      }
   else
      {
      AddInBevel->Visible = true;
      AddInLabel->Caption = addInCaption.c_str();
      AddInLabel->Visible = true;
      }

   // display logo if necessary.
   ApsimSettings settings;
   string fileName;
   settings.read("Outlook Skin|logo", fileName);
   if (fileName != "")
      {
      fileName = getAppHomeDirectory() + "\\" + fileName;
      LogoImage->Picture->LoadFromFile(fileName.c_str());
      }
   }
// ------------------------------------------------------------------
//  Short description:
//    user has closed our dialog - restore all settings if user
//    clicked cancel.

//  Notes:

//  Changes:
//    DPH 29/6/98

// ------------------------------------------------------------------
void __fastcall TDrill_down_form::FormClose(TObject *Sender,
      TCloseAction &Action)
   {
   if (ModalResult == mrOk)
      scenarios->save("Default");

   delete ValueSelectPopup;
   }
//---------------------------------------------------------------------------
void __fastcall TDrill_down_form::ClearButtonClick(TObject *Sender)
   {
   scenarios->deleteAllScenarios();
   Refresh();
   }
//---------------------------------------------------------------------------

void __fastcall TDrill_down_form::popupClose(System::TObject* Sender, TCloseAction &Action)
   {
   ValueSelectPopup->FormClose(Sender, Action);
   string selectedScenario;
   if (ScenarioTree->Selected->Parent == NULL)
      selectedScenario = ScenarioTree->Selected->Text.c_str();
   else
      selectedScenario = ScenarioTree->Selected->Parent->Text.c_str();
   if (ValueSelectPopup->applied)
      {
      ValueSelectPopup->applied = false; //flag has been used- turn it off
      scenarios->createScenariosFrom(selectedScenario,
                                     ValueSelectPopup->factorName,
                                     ValueSelectPopup->SelectedItems);
      Refresh();
      }
   else if (ValueSelectPopup->appliedToAll)
      {
      ValueSelectPopup->appliedToAll = false; //flag has been used- turn it off
      scenarios->createScenarioPermutation(selectedScenario,
                                           ValueSelectPopup->factorName,
                                           ValueSelectPopup->SelectedItems);
      Refresh();
      }
   }
//---------------------------------------------------------------------------
void __fastcall TDrill_down_form::ScenarioTreeMouseDown(TObject *Sender,
      TMouseButton Button, TShiftState Shift, int X, int Y)
   {
   int x = ScenarioTree->ClientOrigin.x + X;
   int y = ScenarioTree->ClientOrigin.y + Y;

   TTreeNode* node = ScenarioTree->GetNodeAt(X, Y);
   if (Button == mbRight && node->Level == 0)
      {
      ScenarioTree->Selected = node;
      ScenarioNamePopup->Popup(x,y);
      }
   else if (!weAreExpanding && node != NULL && Button == mbLeft)
      {
      ScenarioTree->Selected = node;
//      ScenarioTree->Selected->Expanded = !ScenarioTree->Selected->Expanded;
      if (node->Parent != NULL)
         {
         // get factor name and value.
         string line = node->Text.c_str();
         string Factor_name;
         string Factor_value;
         int posEquals = line.find('=');
         if (posEquals != string::npos)
            {
            Factor_name = line.substr(0, posEquals);
            stripLeadingTrailing(Factor_name, " ");
            Factor_value = line.substr(posEquals+1);
            stripLeadingTrailing(Factor_value, " ");
            }
         else
            {
            Factor_name = "";
            Factor_value = "";
            }

         // delete old selections.
         if (ValueSelectPopup != NULL)
            delete ValueSelectPopup;
         ValueSelectPopup = new TValueSelectPopup(this);

         // get a list of identifier values that the user can select from.
         string selectedScenario = node->Parent->Text.c_str();
         scenarios->getFactorValues(selectedScenario,
                                    Factor_name,
                                    ValueSelectPopup->SelectedItems);
         ValueSelectPopup->CurrentValue = Factor_value;
         ValueSelectPopup->factorName = Factor_name;

         // put identifier into listview.
         ValueSelectPopup->ListView->Columns->Items[0]->Caption = Factor_name.c_str();

         // display popup.
         ValueSelectPopup->OnClose = popupClose;
         ValueSelectPopup->Left = x;
         ValueSelectPopup->Top = y;
         if (ValueSelectPopup->Left + ValueSelectPopup->Width > Screen->Width)
            ValueSelectPopup->Left = x - ValueSelectPopup->Width;
         if (ValueSelectPopup->Top + ValueSelectPopup->Height > Screen->Height)
            ValueSelectPopup->Top = y - ValueSelectPopup->Height - 10;
         ValueSelectPopup->ShowModal();
         }
      scenarios->makeScenariosValid();
      }
   weAreExpanding = fullExpColl = false;
   }
//---------------------------------------------------------------------------
void __fastcall TDrill_down_form::ShowAllButtonClick(TObject *Sender)
   {
   fullExpColl = true;
   ScenarioTree->FullExpand();
   }
//---------------------------------------------------------------------------
void __fastcall TDrill_down_form::HideAllButtonClick(TObject *Sender)
   {
   fullExpColl = true;
   ScenarioTree->FullCollapse();
   }
//---------------------------------------------------------------------------
void __fastcall TDrill_down_form::Delete1Click(TObject *Sender)
   {
   // delete the current simulation and tab name.
   if (scenarios->count() == 1)
      scenarios->deleteAllScenarios();
   else
      {
      string selectedScenario = ScenarioTree->Selected->Text.c_str();
      if (ScenarioTree->Selected->Parent != NULL)
         selectedScenario = ScenarioTree->Selected->Parent->Text.c_str();

      scenarios->deleteScenario(selectedScenario);
      }
   Refresh();
   }
//---------------------------------------------------------------------------
void __fastcall TDrill_down_form::ScenarioTreeEditing(TObject *Sender,
      TTreeNode *Node, bool &AllowEdit)
   {
   AllowEdit = renamingNode && (Node->Level == 0);
   renamingNode = false;
   }
//---------------------------------------------------------------------------
void __fastcall TDrill_down_form::ScenarioTreeEdited(TObject *Sender,
      TTreeNode *Node, AnsiString &S)
   {
   scenarios->renameScenario(Node->Text.c_str(), S.c_str());
   }
//---------------------------------------------------------------------------
void __fastcall TDrill_down_form::AddInLabelClick(TObject *Sender)
   {
   scenarios->showUI();
   scenarios->makeScenariosValid();
   Refresh();
   }
//---------------------------------------------------------------------------

void __fastcall TDrill_down_form::ScenarioTreeCollapsing(TObject *Sender,
      TTreeNode *Node, bool &AllowCollapse)
   {
   if (!fullExpColl) weAreExpanding = true;
   }
//---------------------------------------------------------------------------
void __fastcall TDrill_down_form::ScenarioTreeExpanding(TObject *Sender,
      TTreeNode *Node, bool &AllowExpansion)
   {
   if (!fullExpColl) weAreExpanding = true;
   }
//---------------------------------------------------------------------------

void __fastcall TDrill_down_form::Rename1Click(TObject *Sender)
{
   renamingNode = true;
   ScenarioTree->Selected->EditText();
}
//---------------------------------------------------------------------------
void GetListOfScenarios(vector<string>& saved)
   {
   ApsimSettings settings;
   vector<string> sections;
   settings.getSectionNames(sections);

   // Remove all sections that don't start with "Outlook Scenario "
   vector<string> scenarioStrings;
   for_each(sections.begin(), sections.end(),
            MatchPartialStringAndStore<vector<string> >("Outlook Scenario ", scenarioStrings));

   for_each(scenarioStrings.begin(), scenarioStrings.end(),
            RemoveSubStringAndStore<vector<string> >("Outlook Scenario ", saved));
   }
//---------------------------------------------------------------------------
void __fastcall TDrill_down_form::SaveLabelClick(TObject *Sender)
   {
   vector<string> saved;
   GetListOfScenarios(saved);
   AnsiString value;
   if (InputQuery("Save name", "Save scenarios under what name?", value))
      {
      bool doSave;
      if (find(saved.begin(), saved.end(), value.c_str()) == saved.end())
         {
         saved.push_back(value.c_str());
         doSave = true;
         }
      else
         doSave = (MessageDlg("Overwrite existing scenario set?", mtConfirmation,
                              TMsgDlgButtons() << mbYes << mbNo, 0) == mrYes);

      if (doSave)
         scenarios->save(value.c_str());
      }
   }
//---------------------------------------------------------------------------
void __fastcall TDrill_down_form::RestoreLabelClick(TObject *Sender)
   {
   vector<string> saved;
   GetListOfScenarios(saved);
   Stl_2_tstrings(saved, ScenarioSelectForm->ScenarioList->Items);
   if (ScenarioSelectForm->ShowModal())
      {
      scenarios->restore(ScenarioSelectForm->ScenarioList->Text.c_str());
      Refresh();
      Caption = "Scenario set: " + ScenarioSelectForm->ScenarioList->Text;
      }
   }
//---------------------------------------------------------------------------
// User has begun dragging a node in the scenario Tree - allow a drop here?
//---------------------------------------------------------------------------
void __fastcall TDrill_down_form::ScenarioTreeDragOver(TObject *Sender,
      TObject *Source, int X, int Y, TDragState State, bool &Accept)
   {
   TTreeNode* nodeOver = ScenarioTree->GetNodeAt(X, Y);
   Accept = (ScenarioTree->Selected->Level == 0
             && (nodeOver == NULL || nodeOver->Level == 0));
   }
//---------------------------------------------------------------------------
// User has dropped a scenario tree node.
//---------------------------------------------------------------------------
void __fastcall TDrill_down_form::ScenarioTreeDragDrop(TObject *Sender,
      TObject *Source, int X, int Y)
   {
   TTreeNode* sourceNode = ScenarioTree->Selected;

   TTreeNode* nodeOver = ScenarioTree->GetNodeAt(X, Y);
   TTreeNode* newNode = ScenarioTree->Items->Insert(nodeOver, sourceNode->Text);
   TTreeNode* childNode = sourceNode->getFirstChild();
   while (childNode != NULL)
      {
      ScenarioTree->Items->AddChild(newNode, childNode->Text);
      childNode = sourceNode->GetNextChild(childNode);
      }

   scenarios->moveScenario(sourceNode->Text.c_str(), nodeOver->Text.c_str());
   ScenarioTree->Items->Delete(ScenarioTree->Selected);
   }
//---------------------------------------------------------------------------

