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
#include <map>
#include "Scenarios.h"

//---------------------------------------------------------------------------
class TDrill_down_form : public TForm
{
__published:	// IDE-managed Components
   TTabControl *Tab_control;
   TImageList *ImageList;
   TPanel *Panel1;
   TBitBtn *Ok_button;
   TBitBtn *Cancel_button;
   TScrollBox *ScrollBox;
   TSpeedButton *SpeedButton1;
   TSpeedButton *SpeedButton2;
   TSpeedButton *SpeedButton3;
   TSpeedButton *SpeedButton4;
   TSpeedButton *SpeedButton5;
   TSpeedButton *SpeedButton6;
   TSpeedButton *SpeedButton7;
   TSpeedButton *SpeedButton8;
   TSpeedButton *SpeedButton9;
   TSpeedButton *SpeedButton10;
   TSpeedButton *SpeedButton11;
   TSpeedButton *SpeedButton12;
   TSpeedButton *SpeedButton13;
   TSpeedButton *SpeedButton14;
   TSpeedButton *SpeedButton15;
   TSpeedButton *SpeedButton16;
   TSpeedButton *SpeedButton17;
   TSpeedButton *SpeedButton18;
   TSpeedButton *SpeedButton19;
   TSpeedButton *SpeedButton20;
   TBitBtn *ClearButton;
   TPopupMenu *TabPopup;
   TMenuItem *Rename1;
   TMenuItem *Delete1;
   TPopupMenu *EmptyPopup;
   void __fastcall FormShow(TObject *Sender);
   void __fastcall Tab_controlChange(TObject *Sender);
   void __fastcall FormClose(TObject *Sender, TCloseAction &Action);
   void __fastcall ButtonClick(TObject *Sender);
   void __fastcall FormResize(TObject *Sender);
   void __fastcall ClearButtonClick(TObject *Sender);
   void __fastcall Rename1Click(TObject *Sender);
   void __fastcall Delete1Click(TObject *Sender);
   void __fastcall Tab_controlMouseDown(TObject *Sender,
          TMouseButton Button, TShiftState Shift, int X, int Y);
private:	// User declarations

   bool Is_variable_attribute (const char* name);

   void Refresh (void);
   void RefreshScrollBox(void);
   void Create_tabs (void);
   std::string Get_identifier_from_cell (int Col, int Row);
   std::string Get_value_from_cell (int Col, int Row);

public:		// User declarations
   __fastcall TDrill_down_form(TComponent* Owner);

   Scenarios* scenarios;

   void SetPresentationFonts(bool LargeFonts);
};
//---------------------------------------------------------------------------
extern TDrill_down_form *Drill_down_form;
//---------------------------------------------------------------------------
#endif
