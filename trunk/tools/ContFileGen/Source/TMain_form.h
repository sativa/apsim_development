//---------------------------------------------------------------------------
#ifndef TMain_formH
#define TMain_formH
//---------------------------------------------------------------------------
#include <vcl\Classes.hpp>
#include <vcl\Controls.hpp>
#include <vcl\StdCtrls.hpp>
#include <vcl\Forms.hpp>
#include "Grids.hpp"
#include <vcl\Buttons.hpp>
#include <vcl\Dialogs.hpp>
#include <general\string_functions.h>
#include <fstream.h>
const int MAX_OSTREAMS = 50;
//---------------------------------------------------------------------------
class TMain_form : public TForm
{
__published:	// IDE-managed Components
   TStringGrid *Replacement_grid;
   TEdit *Control_file_template;
   TLabel *Label1;
   TLabel *Label2;
   TEdit *Output_file;
   TButton *Browse_button1;
   TButton *Browse_button2;
   TLabel *Label3;
   TBitBtn *BitBtn1;
   TBitBtn *Ok_button;
   TOpenDialog *Open_dialog;
   TSaveDialog *Save_dialog;
   TLabel *Label4;
   TLabel *Label5;
   TLabel *Label6;
   TLabel *Label7;
   TStringGrid *Sim_range_grid;
   TLabel *Label8;
   TLabel *Label9;
   TButton *Load_button;
   TButton *Save_button;
   TOpenDialog *OpenDialog;
   TSaveDialog *SaveDialog;
   TLabel *Label10;
   TEdit *Start_sim_number;
   void __fastcall Browse_button1Click(TObject *Sender);
   void __fastcall Browse_button2Click(TObject *Sender);
   void __fastcall Ok_buttonClick(TObject *Sender);
   void __fastcall FormClose(TObject *Sender, TCloseAction &Action);
   void __fastcall Load_buttonClick(TObject *Sender);
   void __fastcall Save_buttonClick(TObject *Sender);
private:	// User declarations
   ofstream Out_streams[MAX_OSTREAMS];
   string Current_simulation, Template;
   long Current_simulation_number;
   void Permutation(int row);
   void Do_replacement (string& Macro_string, string& Search_string, string& Replacement_string);
   void __fastcall Load_settings(const char* File_name);
   void __fastcall Save_settings(const char* File_name);
   void Open_all_output_streams (void);
   void Close_all_output_streams (void);
   void Write_section(int Current_simulation_number);

public:		// User declarations
   __fastcall TMain_form(TComponent* Owner);
   __fastcall ~TMain_form();
};
//---------------------------------------------------------------------------
extern TMain_form *Main_form;
//---------------------------------------------------------------------------
#endif
