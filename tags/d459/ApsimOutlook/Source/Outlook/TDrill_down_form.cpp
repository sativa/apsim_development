//---------------------------------------------------------------------------
#include <vcl\vcl.h>
#pragma hdrstop

#include "TDrill_down_form.h"
#include "TValueSelectionForm.h"
#include "TTabRenameForm.h"
#include <general\vcl_functions.h>
#include <general\math_functions.h>
#include <general\string_functions.h>
#include <general\path.h>

#include <components\general\tmultistringlist.h>
#include <general\ini_file.h>
#include <strstream>
//---------------------------------------------------------------------------
#pragma link "Grids"
#pragma resource "*.dfm"
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
void TDrill_down_form::Create_tabs (void)
   {
   vector<string> Names;
   scenarios->getScenarioNames(Names);
   Stl_2_tstrings (Names, Tab_control->Tabs);

   string current = scenarios->getCurrentScenario();
   Tab_control->TabIndex = Tab_control->Tabs->IndexOf(current.c_str());
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
   Create_tabs();
   RefreshScrollBox();
   if (Tab_control->Tabs->Count == 1)
      Delete1->Enabled = false;
   else
      Delete1->Enabled = true;
   }

// ------------------------------------------------------------------
//  Short description:
//    Refresh scroll box.

//  Notes:

//  Changes:
//    DPH 29/6/98
//    DPH 1/6/99 changed to a scroll box instead of a grid.

// ------------------------------------------------------------------
void TDrill_down_form::RefreshScrollBox (void)
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



// ------------------------------------------------------------------
//  Short description:
//    Form has been resized.  Refresh everything.

//  Notes:

//  Changes:
//    DPH 29/6/98

// ------------------------------------------------------------------
void __fastcall TDrill_down_form::FormResize(TObject *Sender)
   {
   static const int BUTTON_HEIGHT = 60;

   for (int i = 0; i < ScrollBox->ControlCount; i++)
      {
      ScrollBox->Controls[i]->Left = 0;
      ScrollBox->Controls[i]->Top = i * BUTTON_HEIGHT;
      ScrollBox->Controls[i]->Width = ScrollBox->ClientWidth;
      ScrollBox->Controls[i]->Height = BUTTON_HEIGHT;
      }
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
   Refresh();
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
//   if (ModalResult != mrOk)
//      Restore_simulations();
   }

// ------------------------------------------------------------------
//  Short description:
//    user has clicked a button.  Go display all values.

//  Notes:

//  Changes:
//    DPH 1/6/99

// ------------------------------------------------------------------
void __fastcall TDrill_down_form::ButtonClick(TObject *Sender)
   {
   TSpeedButton* Button = dynamic_cast<TSpeedButton*> (Sender);

   // get identifier from button.
   string Factor_name = Button->Caption.c_str();
   Factor_name.erase (Factor_name.find(" - "));

   string Factor_value;
   Graphics::TBitmap* temp;
   scenarios->getFactorAttributes(Factor_name, Factor_value, temp);

   TValueSelectionForm* ValSelectionForm  =
                                scenarios->getUIForm(Factor_name,Application->MainForm) ;
   if (ValSelectionForm == NULL)
      ValSelectionForm = ValueSelectionForm;

   // delete old selections.
   ValSelectionForm->SelectedItems.erase(ValSelectionForm->SelectedItems.begin(),
                                           ValSelectionForm->SelectedItems.end());

   // get a list of identifier values that the user can select from.
   scenarios->getFactorValues(Factor_name, ValSelectionForm->SelectedItems);
   ValSelectionForm->CurrentValue = Factor_value;

   // put identifier into listview.
   ValSelectionForm->ListView->Columns->Items[0]->Caption = Factor_name.c_str();

   // display form.
   if (ValSelectionForm->ShowModal() == mrOk)
      {
      // User has clicked ok.  Create the multiple simulations.
      if (ValSelectionForm->ChangeCurrentRadio->Checked)
         scenarios->createScenariosFromCurrent
            (Factor_name.c_str(), ValSelectionForm->SelectedItems);
      else
         scenarios->createScenarioPermutation
            (Factor_name.c_str(), ValSelectionForm->SelectedItems);
      Refresh();
      }
   }

// ------------------------------------------------------------------
//  Short description:
//    User has clicked a tab.  Refresh just the grid.

//  Notes:

//  Changes:
//    DPH 29/6/98

// ------------------------------------------------------------------
void __fastcall TDrill_down_form::Tab_controlChange(TObject *Sender)
   {
   scenarios->setCurrentScenario(Tab_control->Tabs->Strings[Tab_control->TabIndex].c_str());
   Refresh();
   }

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
// ------------------------------------------------------------------
//  Short description:
//    restore all simulations

//  Notes:

//  Changes:
//    DPH 29/6/98

// ------------------------------------------------------------------
/*void TDrill_down_form::Restore_simulations (void)
   {
   Simulations->Selected_simulations->Clear();

   for (list<TSimulation>::iterator i = Saved_simulations.begin();
                                    i != Saved_simulations.end();
                                    i++)
      Simulations->Select_simulation(*i);
   }
*/
// ------------------------------------------------------------------
//  Short description:
//    change the font settings on form.

//  Notes:

//  Changes:
//    DPH 29/6/98

// ------------------------------------------------------------------
void TDrill_down_form::SetPresentationFonts(bool LargeFonts)
   {
   if (LargeFonts)
      {
      ScrollBox->Font->Size = 14;
      ValueSelectionForm->ListView->Font->Size = 14;
      }
   else
      {
      ScrollBox->Font->Size = 10;
      ValueSelectionForm->ListView->Font->Size = 8;
      }
   }
//---------------------------------------------------------------------------
void __fastcall TDrill_down_form::ClearButtonClick(TObject *Sender)
   {
   bool leaveDefault = true;
   scenarios->deleteAllScenarios(leaveDefault);
   Refresh();
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
void __fastcall TDrill_down_form::Delete1Click(TObject *Sender)
   {
   // delete the current simulation and tab name.
   scenarios->deleteCurrentScenario();
   Tab_control->Tabs->Delete(Tab_control->TabIndex);
   Create_tabs();
   Tab_controlChange(Sender);
   }
//---------------------------------------------------------------------------
void __fastcall TDrill_down_form::Tab_controlMouseDown(TObject *Sender,
      TMouseButton Button, TShiftState Shift, int X, int Y)
   {
   // select the clicked tab.
   if (Button == mbRight)
      Tab_control->Perform(WM_LBUTTONDOWN,
                           MK_LBUTTON,
                           MAKELPARAM(X, Y));
   }
//---------------------------------------------------------------------------

