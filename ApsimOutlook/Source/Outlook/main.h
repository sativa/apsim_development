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
#include <ToolWin.hpp>
#include <ImgList.hpp>
#include "StrHlder.hpp"
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
   TMenuItem *FileOpenMenu;
   TMenuItem *FileOpenDatasetMenu;
   TOpenDialog *OpenDialog;
   TMenuItem *HelpContentsMenu;
   TMenuItem *FilePresentationFontsMenu;
   TMenuItem *N1;
   TMenuItem *Evaluate1;
        TControlBar *ControlBar1;
        TToolBar *ToolBar1;
        TToolBar *ToolBar2;
        TToolButton *File_open_button;
        TToolButton *Select_data_button;
        TToolButton *Print_button;
        TToolButton *Font_button;
        TToolButton *ToolButton1;
        TToolButton *Copy_button;
        TToolButton *Copy_without_button;
        TToolButton *Excel_button;
        TToolButton *ToolButton2;
        TToolButton *Evaluate_button;
        TImageList *Main_toolbar_images;
        TImageList *Chart_images;
        TToolButton *Time_series_button;
        TToolButton *Difference_button;
        TToolButton *ToolButton3;
        TToolButton *Pie_button;
        TToolButton *Box_button;
        TToolButton *Frequency_button;
        TToolButton *Probability_button;
        TToolButton *ToolButton4;
        TToolButton *Summary_button;
        TToolButton *XY_button;
        TToolButton *Properties_button;
        TToolButton *ToolButton5;
        TToolButton *Select_simulation_button;
        TToolButton *SOI_button;
   TStrHolder *StrHolder1;
   TToolButton *GM_button;
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
   void __fastcall Evaluate(TObject *Sender);
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
