//---------------------------------------------------------------------------
#include <general\pch.h>
#include <vcl.h>
#pragma hdrstop

#include "TDrill_down_form.h"
#include "TValueSelectionForm.h"
#include "TTabRenameForm.h"
#include "TValueSelectPopup.h"

#include <general\vcl_functions.h>
#include <general\math_functions.h>
#include <general\string_functions.h>
#include <general\path.h>

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
   ScenarioTree->BeginUpdate();

   //capture important state information about the present tree
   AnsiString previous;
   vector<AnsiString> expandedNodes;
   if (ScenarioTree->Selected != NULL)
      {
      if (ScenarioTree->Selected->Level > 0)
         previous = ScenarioTree->Selected->Parent->Text;
      else
         previous = ScenarioTree->Selected->Text;

      // capture the expanded state of every scenario name
      TTreeNode* node = ScenarioTree->Items->Item[0];
      while (node != NULL)
         {
         if (node->Expanded)
            expandedNodes.push_back(node->Text);
         node = node->getNextSibling();
         }
      }
   else
      previous = "";

   ScenarioTree->Items->Clear();
   vector<string> Names;
   scenarios->getScenarioNames(Names);
   for (vector<string>::iterator i = Names.begin(); i!=Names.end(); i++)
      {
      AnsiString nameString = "<B>" + AnsiString(i->c_str()) + "</B>";
      TTreeNode* newScen = ScenarioTree->Items->Add(NULL, nameString);
      vector<string> factorNames;
      scenarios->getFactorNames(factorNames);
      scenarios->setCurrentScenario(*i);
      for (vector<string>::iterator j = factorNames.begin(); j!=factorNames.end(); j++)
         {
         AnsiString valueString = j->c_str();
         valueString += " = <U><A href=\"";
         valueString += j->c_str();
         valueString += "\">";
         string factorValue;
         Graphics::TBitmap* temp;
         scenarios->getFactorAttributes(*j, factorValue, temp);
         valueString += factorValue.c_str();
         valueString += "</A></U>";
         ScenarioTree->Items->AddChild(newScen, valueString);
         }

      }

   // highlight the logically 'current' scenario; ie. the first scenario closest
   // to the node previously highlighted
   int index;
   for (index = 0; index < ScenarioTree->Items->Count; index++)
      {
      if (ScenarioTree->Items->Item[index]->Level == 0 &&
          ScenarioTree->Items->Item[index]->Text.Pos(previous) > 0)
         break;
      }
   if (index < ScenarioTree->Items->Count)
      ScenarioTree->Selected = ScenarioTree->Items->Item[index];
   else
      ScenarioTree->Items->Item[0]->Selected = true;

   ScenarioTree->Selected->Expanded = true;

   // expand any other nodes that were previously expanded
   for (unsigned j = 0; j < expandedNodes.size(); j++)
      {
      TTreeNode* n = ScenarioTree->Items->Item[0];
      while (n != NULL)
         {
         if (find(expandedNodes.begin(), expandedNodes.end(), n->Text) != expandedNodes.end())
            n->Expanded = true;
         n = n->getNextSibling();
         }
      }

   ScenarioTree->EndUpdate();
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
   refreshScenarioTree();
//   RefreshScrollBox();
   }

// ------------------------------------------------------------------
//  Short description:
//    Refresh scroll box.

//  Notes:

//  Changes:
//    DPH 29/6/98
//    DPH 1/6/99 changed to a scroll box instead of a grid.

// ------------------------------------------------------------------
/*void TDrill_down_form::RefreshScrollBox (void)
   {
   vector<string> Factor_names;

   scenarios->getFactorNames(Factor_names);
   for (unsigned int i = 0; i < Factor_names.size(); i++) {
      string val;
      Graphics::TBitmap* pic;
      scenarios->getFactorAttributes(Factor_names[i],val,pic);

      string Caption = Factor_names[i] + " - " + val;
      TSpeedButton* Button = dynamic_cast <TSpeedButton*> (ScrollBox->Controls[i]);
      Button->Caption = Caption.c_str();
      Button->Glyph->Assign(pic);
      Button->Visible = true;
   }
   for (int i = Factor_names.size(); i < ScrollBox->ControlCount; i++)
      ScrollBox->Controls[i]->Visible = false;

   }

*/


// ------------------------------------------------------------------
//  Short description:
//    form about to be shown.  initialise everything.

//  Notes:

//  Changes:
//    DPH 29/6/98

// ------------------------------------------------------------------
void __fastcall TDrill_down_form::FormShow(TObject *Sender)
   {
   ValueSelectPopup = new TValueSelectPopup(this);

   Refresh();
//   ScenarioTree->Items->Item[1]->Expanded = true;
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
   delete ValueSelectPopup;
//   if (ModalResult != mrOk)
//      Restore_simulations();
   }


// ------------------------------------------------------------------
//  Short description:
//    User has clicked a tab.  Refresh just the grid.

//  Notes:

//  Changes:
//    DPH 29/6/98

// ------------------------------------------------------------------
/*void __fastcall TDrill_down_form::Tab_controlChange(TObject *Sender)
   {
   scenarios->setCurrentScenario(Tab_control->Tabs->Strings[Tab_control->TabIndex].c_str());
   Refresh();
   }
*/
// ------------------------------------------------------------------
//  Short description:
//    save all simulations

//  Notes:

//  Changes:
//    DPH 29/6/98

// ------------------------------------------------------------------
/*void TDrill_down_form::Save_scenarios (void)
   {
   Saved_scenarios.erase(Saved_scenarios.begin(), Saved_scenarios.end());

   // save the simulations just in case the user presses cancel.
   list<string> Simulation_names;
   Simulations->Get_selected_simulation_names (Simulation_names);

   TSimulation Simulation;
   for (list<string>::iterator i = Simulation_names.begin();
                               i != Simulation_names.end();
                               i++)
      {
      Simulations->Get_selected_simulation ( (*i).c_str(), Simulation);
      Saved_simulations.push_back (Simulation);
      }
   }
*/
//---------------------------------------------------------------------------
void __fastcall TDrill_down_form::ClearButtonClick(TObject *Sender)
   {
   bool leaveDefault = true;
   scenarios->deleteAllScenarios(leaveDefault);
   Refresh();
   }
//---------------------------------------------------------------------------
/*void __fastcall TDrill_down_form::Delete1Click(TObject *Sender)
   {
   // delete the current simulation and tab name.
   scenarios->deleteCurrentScenario();
   Tab_control->Tabs->Delete(Tab_control->TabIndex);
   refreshScenarioTree();
   Tab_controlChange(Sender);
   }*/
//---------------------------------------------------------------------------


void __fastcall TDrill_down_form::ScenarioTreeParamClick(TObject *Sender,
      TTreeNode *ANode, AnsiString href, AnsiString &value)
{
   AnsiString plainText = ANode->Parent->Text;
   plainText = plainText.SubString(4, plainText.Length()-7);
   scenarios->setCurrentScenario(plainText.c_str());
   // get identifier from button.
   string Factor_name = href.c_str();

   string Factor_value;
   Graphics::TBitmap* temp;
   scenarios->getFactorAttributes(Factor_name, Factor_value, temp);

   // delete old selections.
   ValueSelectPopup->SelectedItems.erase(ValueSelectPopup->SelectedItems.begin(),
                                           ValueSelectPopup->SelectedItems.end());

   // get a list of identifier values that the user can select from.
   scenarios->getFactorValues(Factor_name, ValueSelectPopup->SelectedItems);
   ValueSelectPopup->CurrentValue = Factor_value;
   ValueSelectPopup->factorName = Factor_name;

   // put identifier into listview.
   ValueSelectPopup->ListView->Columns->Items[0]->Caption = Factor_name.c_str();

   // display popup.
   ValueSelectPopup->OnClose = popupClose;

   ValueSelectPopup->Left = mouseDownX;
   ValueSelectPopup->Top = mouseDownY;
   if (ValueSelectPopup->Left + ValueSelectPopup->Width > Screen->Width)
      ValueSelectPopup->Left = mouseDownX - ValueSelectPopup->Width;
   if (ValueSelectPopup->Top + ValueSelectPopup->Height > Screen->Height)
      ValueSelectPopup->Top = mouseDownY - ValueSelectPopup->Height - 10;

   ValueSelectPopup->Show();

}
//---------------------------------------------------------------------------

void __fastcall TDrill_down_form::popupClose(System::TObject* Sender, TCloseAction &Action)
   {
   ValueSelectPopup->FormClose(Sender, Action);
   if (ValueSelectPopup->applied)
      {
      ValueSelectPopup->applied = false; //flag has been used- turn it off
      scenarios->createScenariosFromCurrent
         (ValueSelectPopup->factorName.c_str(), ValueSelectPopup->SelectedItems);
      Refresh();
      }
   else if (ValueSelectPopup->appliedToAll)
      {
      ValueSelectPopup->appliedToAll = false; //flag has been used- turn it off
      scenarios->createScenarioPermutation
         (ValueSelectPopup->factorName.c_str(), ValueSelectPopup->SelectedItems);
      Refresh();
      }
   }


void __fastcall TDrill_down_form::ScenarioTreeMouseDown(TObject *Sender,
      TMouseButton Button, TShiftState Shift, int X, int Y)
{
   mouseDownX = ScenarioTree->ClientOrigin.x + X;
   mouseDownY = ScenarioTree->ClientOrigin.y + Y;

   TTreeNode* hitNode = ScenarioTree->GetNodeAt(X, Y);
   if (hitNode && hitNode->Level == 0)
      {
      ScenarioTree->Selected = hitNode;
      scenarios->setCurrentScenario(hitNode->
                              Text.SubString(4, hitNode->Text.Length()-7).c_str());
      if (Button == mbRight)
         {
         //show popup menu
         }
      else if (Button == mbLeft)
         {
         //expand the node
         hitNode->Expanded = !hitNode->Expanded;
         }
      }
}
//---------------------------------------------------------------------------


void __fastcall TDrill_down_form::Rename1Click(TObject *Sender)
{

   // rename the current simulation and tab name.
   string new_name = scenarios->getCurrentScenario();
   TabRenameForm->EditBox->Text = new_name.c_str();

   if (TabRenameForm->ShowModal() == mrOk)
      {
      scenarios->renameCurrentScenario(TabRenameForm->EditBox->Text.c_str());
      Refresh();
      }

}
//---------------------------------------------------------------------------


void __fastcall TDrill_down_form::ShowAllButtonClick(TObject *Sender)
{
   ScenarioTree->FullExpand();
}
//---------------------------------------------------------------------------

void __fastcall TDrill_down_form::HideAllButtonClick(TObject *Sender)
{
   ScenarioTree->FullCollapse();   
}
//---------------------------------------------------------------------------

void __fastcall TDrill_down_form::Delete1Click(TObject *Sender)
{
   // delete the current simulation and tab name.
   if (scenarios->count() == 1)
      scenarios->deleteAllScenarios(true);
   else
      scenarios->deleteCurrentScenario();
   Refresh();
}
//---------------------------------------------------------------------------

