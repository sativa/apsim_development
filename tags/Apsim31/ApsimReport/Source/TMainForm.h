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
#include <ExtCtrls.hpp>
#include <FileCtrl.hpp>
#include "TSEGReport.h"
#include <QuickRpt.hpp>
#include <Chart.hpp>
#include <DBChart.hpp>
#include <ExtCtrls.hpp>
#include <QrTee.hpp>
#include <QuickRpt.hpp>
#include <TeEngine.hpp>
#include <TeeProcs.hpp>
#include <Qrctrls.hpp>
#include <Dialogs.hpp>
#include <ActnList.hpp>
#include <ComCtrls.hpp>
#include <ImgList.hpp>
#include <Menus.hpp>
#include <ToolWin.hpp>
#include "TSEGLibrary.h"
#include "MRUFList.hpp"
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
   TAction *New;
   TAction *Open;
   TAction *Save;
   TAction *SaveAs;
   TAction *Exit;
   TAction *AdvancedEditMode;
   TAction *ViewExplorer;
   TAction *DeleteFile;
   TAction *CopyToClipboard;
   TAction *PasteFile;
   TAction *RenameFile;
   TPopupMenu *FilePopupMenu;
   TMenuItem *Rename1;
   TMenuItem *Delete1;
   TMenuItem *Copy1;
   TMenuItem *Paste1;
   TSEGReport *SEGReport1;
   TControlBar *ControlBar1;
   TToolBar *Standard;
   TToolButton *ToolButton1;
   TToolButton *ToolButton2;
   TToolButton *ToolButton3;
   TToolButton *ToolButton7;
   TEdit *ZoomEdit;
   TAction *SaveEnvironment;
   TMenuItem *SaveEnvironment1;
   TAction *LayoutMode;
   TToolButton *ToolButton5;
   TAction *PageSetup;
   TMenuItem *PageSetup1;
   TMenuItem *N4;
   TAction *Print;
   TMenuItem *Print1;
   TToolButton *ToolButton6;
   TUpDown *ZoomUpDown;
   TAction *SendToLibrary;
   TAction *Library;
   TMenuItem *Library1;
   TMenuItem *SendToLibrary1;
   TMenuItem *Library2;
   TMenuItem *N5;
   TSEGLibrary *SEGLibrary1;
   TAction *RefreshAction;
   TToolButton *ToolButton4;
   TMenuItem *Refresh1;
   TMenuItem *Copy2;
   TMenuItem *N3;
   TToolButton *ToolButton8;
   TdfsMRUFileList *MRUFileList;
   void __fastcall AdvancedEditModeExecute(TObject *Sender);
   void __fastcall FormShow(TObject *Sender);
   void __fastcall ExitExecute(TObject *Sender);
   void __fastcall OpenExecute(TObject *Sender);
   void __fastcall SaveExecute(TObject *Sender);
   void __fastcall SaveAsExecute(TObject *Sender);
   void __fastcall ZoomEditKeyDown(TObject *Sender, WORD &Key,
          TShiftState Shift);
   void __fastcall NewExecute(TObject *Sender);
   void __fastcall DeleteFileExecute(TObject *Sender);
   void __fastcall CopyToClipboardExecute(TObject *Sender);
   void __fastcall PasteFileExecute(TObject *Sender);
   void __fastcall RenameFileExecute(TObject *Sender);
   void __fastcall SaveEnvironmentExecute(TObject *Sender);
   void __fastcall LayoutModeExecute(TObject *Sender);
   void __fastcall PageSetupExecute(TObject *Sender);
   void __fastcall PrintExecute(TObject *Sender);
   void __fastcall ZoomEditChange(TObject *Sender);
   void __fastcall SendToLibraryExecute(TObject *Sender);
   void __fastcall LibraryExecute(TObject *Sender);
   void __fastcall FormClose(TObject *Sender, TCloseAction &Action);
   void __fastcall RefreshActionExecute(TObject *Sender);
   void __fastcall MRUFileListMRUItemClick(TObject *Sender,
          AnsiString AFilename);
private:	// User declarations
   AnsiString filename;
   AnsiString fileThatWasCopied;
   void open(AnsiString file);
   void save(AnsiString file);
   void saveIfNecessary(void);
   void setCaption(void);
   int textToZoom(AnsiString zoomText);
   void processCommandLine(AnsiString commandLine);

public:		// User declarations
   __fastcall TMainForm(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TMainForm *MainForm;
//---------------------------------------------------------------------------
#endif
