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
#pragma link "TSelected_simulations"
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
//    load in all bitmaps

//  Notes:

//  Changes:
//    DPH 13/1/98

// ------------------------------------------------------------------
void TDrill_down_form::Load_all_bitmaps(string& Bitmap_section_contents)
   {
   ImageList->Clear();
   
   Path p(Application->ExeName.c_str());

   // loop through all lines in section.
   istrstream in (Bitmap_section_contents.c_str());
   while (!in.eof())
      {
      string line;
      getline(in, line);
      if (line.length() > 0)
         {
         string Attribute_name, Bitmap_name;
         Get_keyname_and_value (line.c_str(), Attribute_name, Bitmap_name);
         p.Set_name (Bitmap_name.c_str());
         To_lower(Attribute_name);

         Graphics::TBitmap* bitmap = new Graphics::TBitmap;
         bitmap->LoadFromFile (p.Get_path().c_str());
         int Attribute_index = ImageList->Add(bitmap, NULL);
         delete bitmap;
         Bitmap_indexes.insert (String_int_map::value_type(Attribute_name, Attribute_index));
         }
      }
   }

// ------------------------------------------------------------------
//  Short description:
//    return true if the specified attribute is a variable attribute.

//  Notes:

//  Changes:
//    DPH 29/6/98

// ------------------------------------------------------------------
int TDrill_down_form::Get_bitmap_index_for_identifier (const char* name)
   {
   string Lower_name = name;
   To_lower(Lower_name);
   
   String_int_map::iterator i = Bitmap_indexes.find (Lower_name);
   if (i != Bitmap_indexes.end())
      return (*i).second;
   else
      return -1;
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
   AnsiString CurrentTabName;
   if (Tab_control->TabIndex >= 0)
      CurrentTabName = Tab_control->Tabs->Strings[Tab_control->TabIndex];

   list<string> Names;
   Simulations->Get_selected_simulation_names (Names);
   Stl_2_tstrings (Names, Tab_control->Tabs);

   Tab_control->TabIndex = Tab_control->Tabs->IndexOf(CurrentTabName);
   if (Tab_control->TabIndex == -1)
      Tab_control->TabIndex = 0;  
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
   Graphics::TBitmap* EmptyBitmap = new Graphics::TBitmap;

   vector<string> Identifiers, Values;

   string CurrentSimulationName = Tab_control->Tabs->Strings[Tab_control->TabIndex].c_str();

   Simulations->Get_selected_simulation (CurrentSimulationName.c_str(),
                                         Current_simulation);
   Current_simulation.Get_identifiers_and_values (Identifiers, Values);

   // put identifiers in first column and values in second column
   for (unsigned int i = 0; i < Values.size(); i++)
      {
      // give button a caption
      string Caption;
      if (Identifiers[i] != "")
         Caption = Identifiers[i] + " - ";
      Caption += Values[i];
      TSpeedButton* Button = dynamic_cast <TSpeedButton*> (ScrollBox->Controls[i]);
      Button->Caption = Caption.c_str();

      // give button a bitmap.
      int BitmapIndex = Get_bitmap_index_for_identifier (Identifiers[i].c_str());
      if (BitmapIndex != -1)
         ImageList->GetBitmap(BitmapIndex, Button->Glyph);
      else
         Button->Glyph->Assign(EmptyBitmap);

      Button->Visible = true;
      }
   for (int i = Identifiers.size(); i < ScrollBox->ControlCount; i++)
      ScrollBox->Controls[i]->Visible = false;

   delete EmptyBitmap;
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
   // locate and open our ini file.
   Path Ini_path (Application->ExeName.c_str());
   Ini_path.Set_extension (".ini");
   Ini_file Ini;
   Ini.Set_file_name (Ini_path.Get_path().c_str());

   // read in all bitmaps.
   string Bitmap_section_contents;
   Ini.Read_section_contents (BITMAPS_SECTION, Bitmap_section_contents);
   Load_all_bitmaps (Bitmap_section_contents);

   Save_simulations();
   if (Simulations->Selected_simulations->Count() == 0)
      Simulations->Select_default_simulation();
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
   if (ModalResult != mrOk)
      Restore_simulations();
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
   string Identifier = Button->Caption.c_str();
   string Value = Button->Caption.c_str();
   int posDash = Identifier.find(" - ");
   if (posDash != string::npos)
      {
      Identifier.erase (posDash);
      Value = Value.substr(posDash + 3);
      }
   else
      Identifier = "";

   // delete old selections.
   ValueSelectionForm->SelectedItems.erase(ValueSelectionForm->SelectedItems.begin(),
                                           ValueSelectionForm->SelectedItems.end());

   // get a list of identifier values that the user can select from.
   Simulations->Get_valid_values_for_identifier (Current_simulation,
                                                 Identifier.c_str(),
                                                 ValueSelectionForm->SelectedItems);
   ValueSelectionForm->CurrentValue = Value;

   // put identifier into listview.
   ValueSelectionForm->ListView->Columns->Items[0]->Caption = Identifier.c_str();

   // display form.
   if (ValueSelectionForm->ShowModal() == mrOk)
      {
      // User has clicked ok.  Create the multiple simulations.
      Select_multiple_simluations_permutation (Identifier.c_str(), ValueSelectionForm->SelectedItems);
      }
   }

// ------------------------------------------------------------------
//  Short description:
//    select a permutation of simulations based on the specified
//    identifier, the multiple values it will have and the currently
//    selected simulations.

//  Notes:

//  Changes:
//    DPH 29/6/98
//    DAH 13/2/01: RemoveExistingRadio option no longer needed

// ------------------------------------------------------------------
void TDrill_down_form::Select_multiple_simluations_permutation (const char* Selected_identifier,
                                                                vector<string>& Multiple_values)
   {
/*   if (ValueSelectionForm->RemoveExistingRadio->Checked)
      {
      Simulations->Selected_simulations->Clear();
      Current_simulation.Set_name ("default");
      Select_multiple_simulations (Current_simulation, Selected_identifier, Multiple_values);
      }
   else*/
   if (ValueSelectionForm->ChangeCurrentRadio->Checked)
      {
      Simulations->Unselect_simulation(Current_simulation);
      Select_multiple_simulations (Current_simulation, Selected_identifier, Multiple_values);
      }
   else
      {
      // loop through all currently selected simulations.  For each, create
      // a series of simulations.
      list<TSimulation> SavedSimulations;
      for (int tab = 0; tab < Tab_control->Tabs->Count; tab++)
         {
         string CurrentSimulationName = Tab_control->Tabs->Strings[tab].c_str();
         Simulations->Get_selected_simulation (CurrentSimulationName.c_str(),
                                               Current_simulation);
         SavedSimulations.push_back (Current_simulation);
         }

      Simulations->Selected_simulations->Clear();
      for (list<TSimulation>::iterator sim = SavedSimulations.begin();
                                       sim != SavedSimulations.end();
                                       sim++)
         {
         Select_multiple_simulations ((*sim), Selected_identifier, Multiple_values);
         }
      }
   Refresh();
   }

// ------------------------------------------------------------------
//  Short description:
//    select a series of multiple simulations based on the current
//    simulation and the multiple values it will have.

//  Notes:

//  Changes:
//    DPH 29/6/98
//    dph 8/12/99 added identifier to simulation name - c227,d308

// ------------------------------------------------------------------
void TDrill_down_form::Select_multiple_simulations (TSimulation& Simulation,
                                                    const char* Selected_identifier,
                                                    vector<string>& Multiple_values)
   {
   // loop through all multiple values and select a simulation for each value.
   for (vector<string>::iterator i = Multiple_values.begin();
                                 i != Multiple_values.end();
                                 i++)
      {
      TSimulation New_simulation = Simulation;

      Simulations->Make_simulation_valid (New_simulation, Selected_identifier, (*i).c_str());
      if (Multiple_values.size() > 1)
         {
         string NewName = New_simulation.Get_name();
         if (Str_i_Eq(NewName, "default"))
            NewName = "";
         else
            NewName = NewName + ";";
         NewName += string(Selected_identifier);
         if (NewName != "")
            NewName += "=";
         NewName += *i;
         New_simulation.Set_name (NewName.c_str());
         }
      Simulations->Select_simulation (New_simulation);
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
   RefreshScrollBox();
   }

// ------------------------------------------------------------------
//  Short description:
//    save all simulations

//  Notes:

//  Changes:
//    DPH 29/6/98

// ------------------------------------------------------------------
void TDrill_down_form::Save_simulations (void)
   {
   Saved_simulations.erase(Saved_simulations.begin(), Saved_simulations.end());

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
// ------------------------------------------------------------------
//  Short description:
//    restore all simulations

//  Notes:

//  Changes:
//    DPH 29/6/98

// ------------------------------------------------------------------
void TDrill_down_form::Restore_simulations (void)
   {
   Simulations->Selected_simulations->Clear();

   for (list<TSimulation>::iterator i = Saved_simulations.begin();
                                    i != Saved_simulations.end();
                                    i++)
      Simulations->Select_simulation(*i);
   }
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
   Simulations->Selected_simulations->Clear();
   Current_simulation.Set_name ("default");
   Simulations->Select_simulation(Current_simulation);
   Refresh();
   }
//---------------------------------------------------------------------------
void __fastcall TDrill_down_form::Rename1Click(TObject *Sender)
   {
   // rename the current simulation and tab name.
   TabRenameForm->EditBox->Text = Current_simulation.Get_name().c_str();
   if (TabRenameForm->ShowModal() == mrOk)
      {
      Simulations->Unselect_simulation(Current_simulation);
      Current_simulation.Set_name(TabRenameForm->EditBox->Text.c_str());
      Simulations->Select_simulation(Current_simulation);

      Tab_control->Tabs->Strings[Tab_control->TabIndex] = Current_simulation.Get_name().c_str();
      }
   }
//---------------------------------------------------------------------------
void __fastcall TDrill_down_form::Delete1Click(TObject *Sender)
   {
   // delete the current simulation and tab name.
   Simulations->Unselect_simulation(Current_simulation);
   Tab_control->Tabs->Delete(Tab_control->TabIndex);
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

