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
#include "TSelected_simulations.h"
#include <vcl\Buttons.hpp>
#include <ExtCtrls.hpp>
#include <ImgList.hpp>
#include <Menus.hpp>
#include <map>
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
   TSimulation Current_simulation;
   list<TSimulation> Saved_simulations;

   list<string> Variable_attributes;
   typedef std::map <string, int, std::less<string> > String_int_map;
   String_int_map Bitmap_indexes;

   void Load_all_bitmaps(string& Bitmap_section_contents);
   bool Is_variable_attribute (const char* name);

   void Refresh (void);
   void RefreshScrollBox(void);
   void Create_tabs (void);
   string Get_identifier_from_cell (int Col, int Row);
   string Get_value_from_cell (int Col, int Row);
   void Get_identifiers_and_values (vector<string>& Identifiers,
                                    vector<string>& Values);
   void Select_multiple_simluations_permutation (const char* Selected_identifier,
                                                 vector<string>& Multiple_values);
   void Select_multiple_simulations (TSimulation& Simulation,
                                     const char* Selected_identifier,
                                     vector<string>& Multiple_values);

   int Get_bitmap_index_for_identifier (const char* name);
   void Save_simulations (void);
   void Restore_simulations (void);
   
public:		// User declarations
   __fastcall TDrill_down_form(TComponent* Owner);

   TSelected_simulations* Simulations;

   void SetPresentationFonts(bool LargeFonts);
};
//---------------------------------------------------------------------------
extern TDrill_down_form *Drill_down_form;
//---------------------------------------------------------------------------
#endif
