//---------------------------------------------------------------------------
#include <vcl\vcl.h>
#pragma hdrstop

#include "TMain_form.h"
#include <general\stream_functions.h>
#include <general\stristr.h>
#include <general\vcl_functions.h>
#include <general\path.h>
#include <strstrea.h>
#include <iomanip.h>
//---------------------------------------------------------------------------
#pragma link "Grids"
#pragma resource "*.dfm"
TMain_form *Main_form;
//---------------------------------------------------------------------------
__fastcall TMain_form::TMain_form(TComponent* Owner)
   : TForm(Owner)
   {
   Replacement_grid->Cells[0][0] = "Search string";
   Replacement_grid->Cells[1][0] = "Replacement strings (space separated)";
   Sim_range_grid->Cells[0][0] = "Start";
   Sim_range_grid->Cells[1][0] = "End";
   Sim_range_grid->Cells[0][1] = "0";
   Sim_range_grid->Cells[1][1] = "100000";
   Load_settings("macros.txt");
   }
//---------------------------------------------------------------------------
__fastcall TMain_form::~TMain_form()
   {
   }
//---------------------------------------------------------------------------
void __fastcall TMain_form::FormClose(TObject *Sender, TCloseAction &Action)
   {
   Save_buttonClick(NULL);
   }
//---------------------------------------------------------------------------
void __fastcall TMain_form::Load_settings(const char* File_name)
   {
   Grid_clear(Replacement_grid);
   ifstream Grid_stream(File_name);
   Grid_input_from_csv (Replacement_grid, Grid_stream);
   }
//---------------------------------------------------------------------------
void __fastcall TMain_form::Save_settings(const char* File_name)
   {
   ofstream Grid_stream(File_name);
   Grid_output_to_csv (Replacement_grid, Grid_stream);
   }
//---------------------------------------------------------------------------
void __fastcall TMain_form::Browse_button1Click(TObject *Sender)
   {
   if (Open_dialog->Execute())
      Control_file_template->Text = Open_dialog->FileName;
   }
//---------------------------------------------------------------------------
void __fastcall TMain_form::Browse_button2Click(TObject *Sender)
   {
   if (Save_dialog->Execute())
      Output_file->Text = Save_dialog->FileName;
   }
//---------------------------------------------------------------------------
void __fastcall TMain_form::Ok_buttonClick(TObject *Sender)
   {
   Current_simulation_number = atoi(Start_sim_number->Text.c_str());

   // display hourglass.
   Screen->Cursor = crHourGlass;

   // load contents of control file template.
   ifstream In_stream (Control_file_template->Text.c_str());
   Read_stream(In_stream, Template);

   Current_simulation = Template;
   Open_all_output_streams ();
   Permutation(1);
   Close_all_output_streams ();

   // display arrow
   Screen->Cursor = crArrow;

   Application->MessageBox("Control files have been successfully generated", "", MB_ICONINFORMATION | MB_OK);
   }
//---------------------------------------------------------------------------
void TMain_form::Permutation(int row)
   {
   // loop through all rows in grid
   if (Replacement_grid->Cells[0][row].Length() > 0 &&
       Replacement_grid->Cells[1][row].Length() > 0)
      {
      string Search_string (Replacement_grid->Cells[0][row].c_str());

      // break up the replacement strings into a list.
      list<string> Replacements;
      string Replacement_line (Replacement_grid->Cells[1][row].c_str());
      Split_string (Replacement_line, " ", Replacements);

      // keep a copy of the current simulation so far.
      string Current_so_far(Current_simulation);

      // loop through each replacement word and perform replacement.
      for (list<string>::iterator i = Replacements.begin();
                                  i != Replacements.end();
                                  i++)
         {
         string Replacement_string (*i);
         Do_replacement(Current_simulation, Search_string, Replacement_string);
         Permutation (row+1);
         Current_simulation = Current_so_far;
         }
      }
   else
      {
      // at end of tail recursion.
      // replace built-in macro sim_number$
      ostrstream Sim_number_stream;
      Sim_number_stream.fill('0');
      Sim_number_stream << setw(3) << Current_simulation_number << ends;
      Do_replacement(Current_simulation, "sim_number$", Sim_number_stream.str());
      delete Sim_number_stream.str();

      // write section if necessary.
      Write_section (Current_simulation_number);

      Current_simulation_number++;
      }
   }
//---------------------------------------------------------------------------
void TMain_form::Do_replacement (string& Macro_string, string& Search_string, string& Replacement_string)
   {
   int Pos = Macro_string.find(Search_string);
   while (Pos != string::npos)
      {
      Macro_string.replace(Pos, Search_string.length(), Replacement_string);
      Pos = Macro_string.find(Search_string);
      }
   }
//---------------------------------------------------------------------------
void __fastcall TMain_form::Load_buttonClick(TObject *Sender)
   {
   if (OpenDialog->Execute())
      Load_settings (OpenDialog->FileName.c_str());
   }
//---------------------------------------------------------------------------
void __fastcall TMain_form::Save_buttonClick(TObject *Sender)
   {
   if (SaveDialog->Execute())
      Save_settings (SaveDialog->FileName.c_str());
   }
//---------------------------------------------------------------------------
void TMain_form::Open_all_output_streams (void)
   {
   int row = 1;
   while (Sim_range_grid->Cells[0][row].Length() > 0 &&
          Sim_range_grid->Cells[1][row].Length() > 0)
      {
      Path p(Output_file->Text.c_str());

      ostrstream file_name;
      file_name << p.Get_name_without_ext() << row << p.Get_extension() << ends;
      Out_streams[row-1].open(file_name.str());
      delete file_name.str();

      row++;
      }
   }
//---------------------------------------------------------------------------
void TMain_form::Close_all_output_streams (void)
   {
   int row = 1;
   while (Sim_range_grid->Cells[0][row].Length() > 0 &&
          Sim_range_grid->Cells[1][row].Length() > 0)
      {
      Out_streams[row-1].close();
      row++;
      }
   }

//---------------------------------------------------------------------------
void TMain_form::Write_section(int Current_simulation_number)
   {
   int row = 1;
   bool Found = false;
   while (Sim_range_grid->Cells[0][row].Length() > 0 &&
          Sim_range_grid->Cells[1][row].Length() > 0 &&
          !Found)
      {
      int start = atol(Sim_range_grid->Cells[0][row].c_str());
      int end   = atol(Sim_range_grid->Cells[1][row].c_str());
      if (Current_simulation_number >= start &&
          Current_simulation_number <= end)
         {
         Out_streams[row-1] << Current_simulation;
         Found = true;
         }
      row++;
      }
   }

