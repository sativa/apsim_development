//----------------------------------------------------------------------------
#ifndef MainH
#define MainH
//----------------------------------------------------------------------------
#include "ChildWin.h"
#include <ComCtrls.hpp>
#include <ExtCtrls.hpp>
#include <Messages.hpp>
#include <Buttons.hpp>
#include <Dialogs.hpp>
#include <StdCtrls.hpp>
#include <Menus.hpp>
#include <Controls.hpp>
#include <Forms.hpp>
#include <Graphics.hpp>
#include <Classes.hpp>
#include <SysUtils.hpp>
#include <Windows.hpp>
#include <System.hpp>
#include <Db.hpp>
#include <DBTables.hpp>
#include "TSimulations.h"
#include "TSimulations_from_mdbs.h"
#include "TB97.hpp"
//----------------------------------------------------------------------------
class TMainForm : public TForm
{
__published:
	TMainMenu *MainMenu1;
   TMenuItem *FileMenu;
	TMenuItem *Window1;
   TMenuItem *HelpMenu;
   TMenuItem *FileExitMenu;
   TMenuItem *Window_cascade_menu;
   TMenuItem *Window_tile_menu;
   TMenuItem *Window_arrange_icons_menu;
   TMenuItem *HelpAboutMenu;
   TMenuItem *Window_minimize_all_menu;
	TStatusBar *StatusBar;
   TMenuItem *FileCloseMenu;
   TMenuItem *FilePrintMenu;
   TMenuItem *N2;
   TPrinterSetupDialog *PrinterSetupDialog;
   TMenuItem *N3;
    TSimulations_from_mdbs *All_simulations;
   TDock97 *Top_dock;
   TDock97 *Left_dock;
   TDock97 *Right_dock;
   TDock97 *Bottom_dock;
   TToolbar97 *Main_toolbar;
   TToolbarButton97 *Select_data_button;
   TToolbarButton97 *File_open_button;
   TToolbarButton97 *EXCEL_button;
   TToolbar97 *Chart_window_toolbar;
   TToolbarButton97 *SOI_button;
   TToolbarButton97 *Select_simulation_button;
   TToolbarButton97 *Time_series_button;
   TToolbarButton97 *Properties_button;
   TToolbarButton97 *XY_button;
   TToolbarButton97 *Summary_button;
   TToolbarButton97 *Probability_button;
   TToolbarButton97 *Frequency_button;
   TToolbarButton97 *Box_button;
   TToolbarButton97 *Pie_button;
   TToolbarButton97 *Difference_button;
   TToolbarSep97 *ToolbarSep971;
   TToolbarSep97 *ToolbarSep972;
   TToolbarSep97 *ToolbarSep973;
   TToolbarButton97 *Copy_button;
   TToolbarButton97 *Print_button;
   TToolbarButton97 *Copy_without_button;
   TToolbarSep97 *ToolbarSep974;
   TMenuItem *FileOpenMenu;
   TMenuItem *FileOpenDatasetMenu;
   TOpenDialog *OpenDialog;
   TMenuItem *HelpContentsMenu;
   TMenuItem *FilePresentationFontsMenu;
   TMenuItem *N1;
	void __fastcall FormCreate(TObject *Sender);
	void __fastcall Window_cascade(TObject *Sender);
	void __fastcall UpdateMenuItems(TObject *Sender);
	void __fastcall Window_tile(TObject *Sender);
	void __fastcall Window_arrange_icons(TObject *Sender);
	void __fastcall File_exit(TObject *Sender);
	void __fastcall Window_minimize_all(TObject *Sender);
	void __fastcall FormDestroy(TObject *Sender);
	void __fastcall Help_about(TObject *Sender);
   void __fastcall FormClose(TObject *Sender, TCloseAction &Action);
   void __fastcall FileCloseMenuClick(TObject *Sender);
   void __fastcall FilePrintMenuClick(TObject *Sender);

   void __fastcall Copy_buttonClick(TObject *Sender);
   void __fastcall Copy_without_buttonClick(TObject *Sender);
   void __fastcall EXCEL_buttonClick(TObject *Sender);

   void __fastcall FileOpenMenuClick(TObject *Sender);
   void __fastcall FileOpenDatasetMenuClick(TObject *Sender);

   void __fastcall HelpContentsMenuClick(TObject *Sender);
   void __fastcall FilePresentationFontsMenuClick(TObject *Sender);


   void __fastcall FormShow(TObject *Sender);
private:
	void __fastcall CreateMDIChild(const String Name);
	void __fastcall ShowHint(TObject *Sender);
   void __fastcall Close_all ();
   void __fastcall Application_minimize (TObject* Sender);

public:
	virtual __fastcall TMainForm(TComponent *Owner);
};
//----------------------------------------------------------------------------
extern TMainForm *MainForm;
extern TMDIChild *__fastcall MDIChildCreate(void);
//----------------------------------------------------------------------------
#endif
