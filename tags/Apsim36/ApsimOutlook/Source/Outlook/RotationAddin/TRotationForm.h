//---------------------------------------------------------------------------

#ifndef TRotationFormH
#define TRotationFormH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include <ExtCtrls.hpp>
#include <Grids.hpp>
#include <Buttons.hpp>
#include <Mask.hpp>
#include <ComCtrls.hpp>
#include "dbslstbox.hpp"
#include "slstbox.hpp"
#include <ImgList.hpp>
#include <Menus.hpp>
#include <vector>
class RotationAddIn;
//---------------------------------------------------------------------------
class TRotationForm : public TForm
{
__published:	// IDE-managed Components
   TSectionListBox *ListBox;
   TImageList *ImageList1;
   TPanel *Panel1;
   TCheckBox *RotationCheck;
   TLabel *Label1;
   TEdit *NumRotationsEdit;
   TUpDown *NumRotationsUpDown;
   TPanel *Panel2;
   TBitBtn *BitBtn1;
   TBitBtn *BitBtn2;
   TLabel *Label2;
   TLabel *Label3;
   TPopupMenu *PopupMenu1;
   TMenuItem *RenameMenuItem;
   TMenuItem *DeleteMenuItem;
   void __fastcall FormShow(TObject *Sender);
   void __fastcall FormClose(TObject *Sender, TCloseAction &Action);
   void __fastcall ListBoxDragOver(TObject *Sender,
          TObject *Source, int X, int Y, TDragState State, bool &Accept);
   void __fastcall ListBoxDragDrop(TObject *Sender, TObject *Source, int X,
          int Y);
   void __fastcall NumRotationsEditChange(TObject *Sender);
   void __fastcall RenameMenuItemClick(TObject *Sender);
   void __fastcall ListBoxMouseUp(TObject *Sender, TMouseButton Button,
          TShiftState Shift, int X, int Y);
   void __fastcall DeleteMenuItemClick(TObject *Sender);
private:	// User declarations
   void moveItem(int fromSectionID, int fromItemID, int toSectionID);

public:		// User declarations
   __fastcall TRotationForm(TComponent* Owner);
   RotationAddIn* rotationAddIn;
};
//---------------------------------------------------------------------------
extern PACKAGE TRotationForm *RotationForm;
//---------------------------------------------------------------------------
#endif
