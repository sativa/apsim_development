//---------------------------------------------------------------------------
#ifndef TDrill_down_formH
#define TDrill_down_formH
//---------------------------------------------------------------------------
#include <vcl\Classes.hpp>
#include <vcl\Controls.hpp>
#include <vcl\StdCtrls.hpp>
#include <vcl\Forms.hpp>
#include "Grids.hpp"
#include <vcl\ComCtrls.hpp>
#include <vcl\DB.hpp>
#include <vcl\DBTables.hpp>
#include <vcl\DBGrids.hpp>
#include <vcl\Buttons.hpp>
#include <ExtCtrls.hpp>
#include <ImgList.hpp>
#include <Menus.hpp>
#include "paramtreeview.hpp"
#include <map>
#include "Scenarios.h"

//---------------------------------------------------------------------------
class TDrill_down_form : public TForm
{
__published:	// IDE-managed Components
   TImageList *ImageList;
   TPanel *Panel1;
   TBitBtn *Ok_button;
   TBitBtn *Cancel_button;
   TPopupMenu *ScenarioNamePopup;
   TMenuItem *Rename1;
   TMenuItem *Delete1;
   TParamTreeview *ScenarioTree;
   TPanel *Panel2;
   TButton *ShowAllButton;
   TButton *HideAllButton;
   TBitBtn *ClearButton;
   void __fastcall FormShow(TObject *Sender);
//   void __fastcall Tab_controlChange(TObject *Sender);
   void __fastcall FormClose(TObject *Sender, TCloseAction &Action);
//   void __fastcall ButtonClick(TObject *Sender);
   void __fastcall ClearButtonClick(TObject *Sender);
   void __fastcall ScenarioTreeParamClick(TObject *Sender,
          TTreeNode *ANode, AnsiString href, AnsiString &value);
   void __fastcall ScenarioTreeMouseDown(TObject *Sender,
          TMouseButton Button, TShiftState Shift, int X, int Y);
   void __fastcall Rename1Click(TObject *Sender);
   void __fastcall ShowAllButtonClick(TObject *Sender);
   void __fastcall HideAllButtonClick(TObject *Sender);
   void __fastcall Delete1Click(TObject *Sender);
//   void __fastcall Rename1Click(TObject *Sender);
//   void __fastcall Delete1Click(TObject *Sender);
//   void __fastcall Tab_controlMouseDown(TObject *Sender,
//          TMouseButton Button, TShiftState Shift, int X, int Y);
private:	// User declarations

   bool Is_variable_attribute (const char* name);

   void Refresh (void);
   void RefreshScrollBox(void);
   void refreshScenarioTree (void);
   std::string Get_identifier_from_cell (int Col, int Row);
   std::string Get_value_from_cell (int Col, int Row);
   void __fastcall popupClose(System::TObject* Sender, TCloseAction &Action);
   int mouseDownX, mouseDownY;


public:		// User declarations
   __fastcall TDrill_down_form(TComponent* Owner);

   Scenarios* scenarios;

//   void SetPresentationFonts(bool LargeFonts);
};
//---------------------------------------------------------------------------
extern TDrill_down_form *Drill_down_form;
//---------------------------------------------------------------------------
#endif
