//---------------------------------------------------------------------------

#ifndef TValueSelectPopupH
#define TValueSelectPopupH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include "POPUPFORMUNIT.h"
#include "paramchklist.hpp"
#include <ComCtrls.hpp>
#include <ExtCtrls.hpp>
#include <ImgList.hpp>
#include <vector>
#include <string>
using std::vector;
using std::string;

//---------------------------------------------------------------------------
class TValueSelectPopup : public TPopupForm
{
__published:	// IDE-managed Components
   TListView *ListView;
   TPanel *Panel1;
   TLabel *applyLabel;
   TLabel *applyToAllLabel;
   TImage *Image;
   TImageList *StateImages;
   void __fastcall ListViewCompare(TObject *Sender, TListItem *Item1,
          TListItem *Item2, int Data, int &Compare);
   void __fastcall FormClose(TObject *Sender, TCloseAction &Action);
   void __fastcall FormShow(TObject *Sender);
   void __fastcall applyLabelClick(TObject *Sender);
   void __fastcall applyToAllLabelClick(TObject *Sender);
   void __fastcall ListViewChange(TObject *Sender, TListItem *Item,
          TItemChange Change);
   void __fastcall FormMouseDown(TObject *Sender, TMouseButton Button,
          TShiftState Shift, int X, int Y);
private:	// User declarations
   TSortType WhatSortType(std::vector<std::string>& items);
   int checkedCount(void);

public:		// User declarations
   __fastcall TValueSelectPopup(TComponent* Owner);
   __fastcall ~TValueSelectPopup()
   {
   }

   std::vector<std::string> SelectedItems;
   std::string CurrentValue, factorName;
   bool applied, appliedToAll;

};

//---------------------------------------------------------------------------
extern PACKAGE TValueSelectPopup *ValueSelectPopup;
//---------------------------------------------------------------------------
#endif
