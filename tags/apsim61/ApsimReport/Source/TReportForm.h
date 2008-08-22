//---------------------------------------------------------------------------

#ifndef TReportFormH
#define TReportFormH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include "TSEGReport.h"
#include "AdvGrid.hpp"
#include "BaseGrid.hpp"
#include "DBAdvGrd.hpp"
#include <DB.hpp>
#include <ExtCtrls.hpp>
#include <Grids.hpp>
#include <ComCtrls.hpp>
#include <Menus.hpp>
//---------------------------------------------------------------------------
class TReportForm : public TForm
{
__published:	// IDE-managed Components
   TTabControl *TabControl;
   TSEGReport *Report;
   TPopupMenu *PopupMenu;
   TMenuItem *CreatePage;
   TMenuItem *DeletePage;
   TMenuItem *RenamePage;
   void __fastcall TabControlChange(TObject *Sender);
   void __fastcall CreatePageClick(TObject *Sender);
   void __fastcall DeletePageClick(TObject *Sender);
   void __fastcall RenamePageClick(TObject *Sender);
   void __fastcall TabControlMouseDown(TObject *Sender,
          TMouseButton Button, TShiftState Shift, int X, int Y);
   void __fastcall TabControlDragOver(TObject *Sender, TObject *Source,
          int X, int Y, TDragState State, bool &Accept);
   void __fastcall TabControlDragDrop(TObject *Sender, TObject *Source,
          int X, int Y);
private:	// User declarations
   int draggedTab;
public:		// User declarations
   __fastcall TReportForm(TComponent* Owner);

   TNotifyEvent onPageChanged;
};
//---------------------------------------------------------------------------
extern PACKAGE TReportForm *ReportForm;
//---------------------------------------------------------------------------
#endif
