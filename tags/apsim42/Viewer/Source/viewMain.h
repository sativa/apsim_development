//---------------------------------------------------------------------------

#ifndef viewMainH
#define viewMainH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include <Dialogs.hpp>
#include <ExtCtrls.hpp>
#include "AdvGrid.hpp"
#include <ActnList.hpp>
#include <Menus.hpp>
#include <StdActns.hpp>
#include <ComCtrls.hpp>
#include <ToolWin.hpp>
#include <ImgList.hpp>
#include "MRUFList.hpp"
//---------------------------------------------------------------------------
class TmainForm : public TForm
{
__published:	// IDE-managed Components
   TMainMenu *MainMenu;
   TMenuItem *fileItem;
   TActionList *ActionList;
   TFileOpen *FileOpen;
   TFileExit *FileExit;
   TMenuItem *Open;
   TMenuItem *N1;
   TMenuItem *Exit1;
   TStatusBar *StatusBar;
   TToolBar *ToolBar1;
   TToolButton *ToolButton1;
   TImageList *ImageList;
   TToolButton *ToolButton4;
   TCoolBar *CoolBar1;
   TToolButton *ToolButton2;
   TMenuItem *ViewItem;
   TMenuItem *Refresh1;
   TdfsMRUFileList *MRUFileList;
   TToolButton *ToolButton3;
   TAction *RefreshView;
   TMenuItem *WindowItem;
   TWindowCascade *WindowCascade1;
   TWindowTileHorizontal *WindowTileHorizontal1;
   TWindowTileVertical *WindowTileVertical1;
   TWindowMinimizeAll *WindowMinimizeAll1;
   TWindowArrange *WindowArrange1;
   TMenuItem *Cascade1;
   TMenuItem *TileHorizontally1;
   TMenuItem *TileVertically1;
   TMenuItem *MinimizeAll1;
   TMenuItem *Arrange1;
   TMenuItem *Excel1;
   TToolButton *ToolButton5;
   TToolButton *ToolButton6;
   TFileSaveAs *FileSaveAsXL;
   TMenuItem *Help1;
   TMenuItem *About1;
   void __fastcall FileOpenAccept(TObject *Sender);
   void __fastcall MRUFileListMRUItemClick(TObject *Sender,
          AnsiString AFilename);
   void __fastcall FormCreate(TObject *Sender);
   void __fastcall RefreshViewExecute(TObject *Sender);
   void __fastcall FormPaint(TObject *Sender);
   void __fastcall FormShow(TObject *Sender);
   void __fastcall FileSaveAsXLAccept(TObject *Sender);
   void __fastcall About1Click(TObject *Sender);
private:	// User declarations
   void __fastcall OpenFile(String fileName);
public:		// User declarations
   __fastcall TmainForm(TComponent* Owner);
protected:
  void __fastcall WMDropFiles(TWMDropFiles& Message);
// ...
BEGIN_MESSAGE_MAP
  MESSAGE_HANDLER(WM_DROPFILES, TWMDropFiles, WMDropFiles);
END_MESSAGE_MAP(TForm)
};
//---------------------------------------------------------------------------
extern PACKAGE TmainForm *mainForm;
//---------------------------------------------------------------------------
#endif
