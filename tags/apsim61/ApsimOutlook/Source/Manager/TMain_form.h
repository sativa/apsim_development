//----------------------------------------------------------------------------
#ifndef TMain_formH
#define TMain_formH
//----------------------------------------------------------------------------
#include <vcl\ComCtrls.hpp>
#include <vcl\ExtCtrls.hpp>
#include <vcl\Buttons.hpp>
#include <vcl\StdCtrls.hpp>
#include <vcl\Dialogs.hpp>
#include <vcl\Menus.hpp>
#include <vcl\Controls.hpp>
#include <vcl\Forms.hpp>
#include <vcl\Graphics.hpp>
#include <vcl\Classes.hpp>
#include <vcl\Windows.hpp>
#include <vcl\System.hpp>
#include <vcl\DB.hpp>
#include <vcl\DBTables.hpp>
#include <vcl\DBGrids.hpp>
#include "Grids.hpp"
#include "TSimulation_database.h"
#include <Db.hpp>
#include <ADODB.hpp>

//----------------------------------------------------------------------------
class TMain_form : public TForm
{
__published:
	TMainMenu *MainMenu;
	TMenuItem *FileMenu;
   TMenuItem *Open_database_menu;
   TMenuItem *Exit_menu;
	TMenuItem *N1;
   TOpenDialog *Database_open_dialog;
	TSaveDialog *SaveDialog;
	TStatusBar *StatusBar;
   TDBGrid *Simulation_name_grid;
   TDataSource *DataSource1;
   TLabel *Prompt_label;
   TMenuItem *N2;
   TMenuItem *Import_simulation_menu;
   TOpenDialog *Simulation_open_dialog;
   TMenuItem *Import_simulation_using_filespec_menu;
   TMenuItem *Batchimportusingfilespec1;
   TADOTable *Index_table;
   TSimulation_database *Simulation_database;
   TMenuItem *Options1;
   TMenuItem *Checkforduplicatesimulations;
	void __fastcall ShowHint(TObject *Sender);
	void __fastcall Exit(TObject *Sender);
	void __fastcall Open_database(TObject *Sender);
	void __fastcall FormCreate(TObject *Sender);
   void __fastcall Import_simulation(TObject *Sender);
   void __fastcall Import_simulation_using_filespec(TObject *Sender);
   void __fastcall Batchimportusingfilespec1Click(TObject *Sender);
   void __fastcall FormShow(TObject *Sender);
   void __fastcall CheckforduplicatesimulationsClick(TObject *Sender);
private:
   TProgressBar* ProgressBar;
   void __fastcall Show_hide_controls(void);
   void __fastcall Import_files (TStringList* files);
   void Import_files_using_filespec (const char* Database_file_name,
                                     const char* Directory,
                                     const char* File_spec);
   void __fastcall ShowProgress
      (TObject *Sender, AnsiString& CurrentFileName, int CurrentFileNumber,
       int TotalFileNumber);
public:
	virtual __fastcall TMain_form(TComponent *AOwner);
};
//----------------------------------------------------------------------------
extern TMain_form *Main_form;
//----------------------------------------------------------------------------
#endif
