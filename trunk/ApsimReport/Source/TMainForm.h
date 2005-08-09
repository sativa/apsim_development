//---------------------------------------------------------------------------

#ifndef TMainFormH
#define TMainFormH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include <ActnList.hpp>
#include <ComCtrls.hpp>
#include <Dialogs.hpp>
#include <ImgList.hpp>
#include <Menus.hpp>
#include <ToolWin.hpp>
#include <FileCtrl.hpp>
#include "MRUFList.hpp"
#include <ExtCtrls.hpp>
#include <SEGReport\report.h>
//---------------------------------------------------------------------------
class TMainForm : public TForm
{
__published:	// IDE-managed Components
   TOpenDialog *OpenDialog1;
   TSaveDialog *SaveDialog1;
   TMainMenu *MainMenu1;
   TMenuItem *File1;
   TMenuItem *Open1;
   TMenuItem *New1;
   TMenuItem *N1;
   TMenuItem *Save1;
   TMenuItem *SaveAs1;
   TMenuItem *Exit1;
   TMenuItem *N2;
   TMenuItem *Edit1;
   TMenuItem *Edit2;
   TImageList *ImageList1;
   TActionList *ActionList1;
   TAction *NewAction;
   TAction *OpenAction;
   TAction *SaveAction;
   TAction *SaveAsAction;
   TAction *ExitAction;
   TAction *CopyToClipboardAction;
   TControlBar *ControlBar1;
   TToolBar *Standard;
   TToolButton *ToolButton1;
   TToolButton *ToolButton2;
   TToolButton *ToolButton3;
   TToolButton *ToolButton7;
   TEdit *ZoomEdit;
   TAction *PageSetupAction;
   TMenuItem *PageSetup1;
   TMenuItem *N4;
   TAction *PrintAction;
   TMenuItem *Print1;
   TUpDown *ZoomUpDown;
   TAction *SendToLibraryAction;
   TAction *LibraryAction;
   TAction *RefreshAction;
   TToolButton *ToolButton4;
   TMenuItem *Refresh1;
   TMenuItem *Copy2;
   TMenuItem *N3;
   TdfsMRUFileList *MRUFileList;
   TMenuItem *Print2;
   TAction *PrintCurrentPageAction;
   TAction *EditDataAction;
   TToolButton *ToolButton9;
   TPanel *LeftDockPanel;
   TSplitter *LeftSplitter;
   TPanel *BottomDockPanel;
   TSplitter *BottomSplitter;
   TToolButton *ToolButton11;
   TAction *EditReportAction;
   TToolButton *ToolButton12;
   TMenuItem *Editdatapage1;
   TMenuItem *N6;
   TPanel *RightDockPanel;
   TSplitter *RightSplitter;
   TToolBar *ReportToolBar;
   TTabControl *TabControl;
   TPopupMenu *PagePopupMenu;
   TMenuItem *Addanewpage1;
   TMenuItem *Deletecurrentpage1;
   TMenuItem *Renamecurrentpage1;
   void __fastcall FormShow(TObject *Sender);
   void __fastcall ExitActionExecute(TObject *Sender);
   void __fastcall OpenActionExecute(TObject *Sender);
   void __fastcall SaveActionExecute(TObject *Sender);
   void __fastcall SaveAsActionExecute(TObject *Sender);
   void __fastcall ZoomEditKeyDown(TObject *Sender, WORD &Key,
          TShiftState Shift);
   void __fastcall NewActionExecute(TObject *Sender);
   void __fastcall CopyToClipboardActionExecute(TObject *Sender);
   void __fastcall PageSetupActionExecute(TObject *Sender);
   void __fastcall PrintActionExecute(TObject *Sender);
   void __fastcall ZoomEditChange(TObject *Sender);
   void __fastcall FormClose(TObject *Sender, TCloseAction &Action);
   void __fastcall RefreshActionExecute(TObject *Sender);
   void __fastcall MRUFileListMRUItemClick(TObject *Sender,
          AnsiString AFilename);
   void __fastcall PrintCurrentPageActionExecute(TObject *Sender);
   void __fastcall FormDockOver(TObject *Sender,
          TDragDockObject *Source, int X, int Y, TDragState State,
          bool &Accept);
   void __fastcall LeftDockPanelGetSiteInfo(TObject *Sender,
          TControl *DockClient, TRect &InfluenceRect, TPoint &MousePos,
          bool &CanDock);
   void __fastcall FormUnDock(TObject *Sender, TControl *Client,
          TWinControl *NewTarget, bool &Allow);
   void __fastcall FormDockDrop(TObject *Sender,
          TDragDockObject *Source, int X, int Y);
   void __fastcall EditReportActionExecute(TObject *Sender);
   void __fastcall EditDataActionExecute(TObject *Sender);
   void __fastcall pageChanged(TObject* sender);

   void __fastcall OnObjectInspectorShow(TObject* sender);
   void __fastcall addMenuItemClick(TObject* sender);
   void __fastcall deleteMenuItemClick(TObject* sender);
   void __fastcall renameMenuItemClick(TObject* sender);
   void __fastcall onMouseDown(TObject* sender, TMouseButton button,
                               TShiftState state, int x, int y);
   void __fastcall onDragOver(TObject* sender, TObject* source,
                              int x, int y, TDragState state,
                              bool &accept);
   void __fastcall onDragDrop(TObject* sender, TObject* source,
                              int x, int y);


private:	// User declarations
   AnsiString filename;
   AnsiString fileThatWasCopied;
   Report report;
   int draggedTab;

   //---------------------------------------------------------------------------
   // Tell report to go into edit mode.
   //---------------------------------------------------------------------------
   void edit(bool turnOn);

   //---------------------------------------------------------------------------
   // Populate the toolbar.
   //---------------------------------------------------------------------------
   void populateToolBar(void);

   void open(AnsiString file, bool quiet = false);
   void save(AnsiString file);
   void saveIfNecessary(void);
   void setCaption(void);
   int textToZoom(AnsiString zoomText);
   void processCommandLine(AnsiString commandLine);

   //---------------------------------------------------------------------------
   // Show dock panel.
   //---------------------------------------------------------------------------
   void ShowDockPanel(TWinControl* APanel, TControl* Client, TRect& dockRect);

   //---------------------------------------------------------------------------
   // Show dock panel.
   //---------------------------------------------------------------------------
   void HideDockPanel(TPanel* APanel);

   //---------------------------------------------------------------------------
   // load and save the position a form from settings file.
   //---------------------------------------------------------------------------
   void loadFormPosition(TForm* form);
   void saveFormPosition(TForm* form);

public:		// User declarations
   __fastcall TMainForm(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TMainForm *MainForm;
//---------------------------------------------------------------------------
#endif
