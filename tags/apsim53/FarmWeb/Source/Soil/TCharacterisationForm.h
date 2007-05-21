//---------------------------------------------------------------------------
#ifndef TCharacterisationFormH
#define TCharacterisationFormH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include <ComCtrls.hpp>
#include <ExtCtrls.hpp>
#include <ImgList.hpp>
#include <ActnList.hpp>
#include <ToolWin.hpp>
#include "AdvGrid.hpp"
#include "BaseGrid.hpp"
#include <Grids.hpp>
#include "AdvCGrid.hpp"
#include <Menus.hpp>
#include <Buttons.hpp>
class Soil;
//---------------------------------------------------------------------------
class __declspec(dllexport) TCharacterisationForm : public TForm
   {
   __published:	// IDE-managed Components
      TPanel *Panel2;
      TPageControl *PageControl;
      TTabSheet *GeneralSheet;
      TAdvStringGrid *GeneralGrid;
      TTabSheet *WaterSheet;
      TAdvStringGrid *WaterGrid;
      TTabSheet *WaterPredictedSheet;
      TAdvStringGrid *WaterPredictedGrid;
      TTabSheet *ProfileSheet;
      TAdvStringGrid *ProfileGrid;
      TTabSheet *APSIMSheet;
      TAdvStringGrid *ApsimGrid;
      TPanel *Panel1;
      TSpeedButton *LockButton;
      TSpeedButton *GraphButton;
    TSpeedButton *NewCropButton;
    TSpeedButton *DeleteCropButton;
      void __fastcall FormClose(TObject *Sender, TCloseAction &Action);
      void __fastcall GridSetEditText(TObject *Sender, int ACol,
          int ARow, const AnsiString Value);
      void __fastcall WaterGridGetCellColor(TObject *Sender, int ARow,
          int ACol, TGridDrawState AState, TBrush *ABrush, TFont *AFont);
      void __fastcall PageControlChanging(TObject *Sender, bool &AllowChange);
      void __fastcall PageControlChange(TObject *Sender);
      void __fastcall GridCanEditCell(TObject *Sender, int ARow,
          int ACol, bool &CanEdit);
      void __fastcall LockLabelClick(TObject *Sender);
      void __fastcall WaterPredictedGridGetCellColor(TObject *Sender,
             int ARow, int ACol, TGridDrawState AState, TBrush *ABrush,
             TFont *AFont);
      void __fastcall GridKeyDown(TObject *Sender, WORD &Key,
             TShiftState Shift);
      void __fastcall GridMouseUp(TObject *Sender, TMouseButton Button,
             TShiftState Shift, int X, int Y);
      void __fastcall GridGetDisplText(TObject *Sender, int ACol, int ARow,
             AnsiString &Value);
      void __fastcall CopyMenuClick(TObject *Sender);
      void __fastcall PasteMenuClick(TObject *Sender);
      void __fastcall GraphButtonClick(TObject *Sender);
      void __fastcall ApsimGridIsFixedCell(TObject *Sender, int ARow,
          int ACol, bool &IsFixed);
    void __fastcall GridProfileCanEdit(TObject *Sender, int ARow, int ACol,
          bool &CanEdit);
    void __fastcall NewCropButtonClick(TObject *Sender);
    void __fastcall DeleteCropButtonClick(TObject *Sender);
   private:	// User declarations
      Soil* soil;

      bool dirtyData;       //This variable is used for dirty data on the current tab
      bool dirtyDataOutside;//Used by soil picker form is data dirty to according to it
      bool readOnly;

      void loadSelectorGrid(void);
      void loadGeneralGrid(void);
      void saveGeneralGrid(void);
      void loadWaterGrid(void);
      void saveWaterGrid(void);
      void loadWaterPredictedGrid(void);
      void loadProfileGrid(void);
      void saveProfileGrid(void);
      void loadApsimGrid(void);
      void saveApsimGrid(void);
      void loadGrids(void);
//      void saveGrids(void);
      void calcAndDisplayPAWC(int col);
      bool colIsLLCol(int col);
      bool colIsPAWCCol(int col);
      void fillFilters();
      void populateGraph();

   virtual void __fastcall CreateParams(Controls::TCreateParams &Params)
      {
//      Params.Style |= WS_CHILD | WS_CLIPSIBLINGS;
//      Params.Style |= WS_TABSTOP | WS_GROUP;
      TForm::CreateParams( Params );
      }

   public:		// User declarations
      __fastcall TCharacterisationForm(TComponent* Owner);
      __fastcall TCharacterisationForm(HWND handle);

      __fastcall ~TCharacterisationForm(void);
      bool isDirty();
      bool isDirtyOutside(){return dirtyDataOutside;}
      void OutsideSave(){dirtyDataOutside = false;}
      void setup(const std::string& soilXML, bool readOnly);
      void setup(Soil* soilNode, bool readonly);
      Soil& getSoil(void) {return *soil;}
      void saveGrids(void);
      void saveSoil(std::string filename);
   };
//---------------------------------------------------------------------------
extern PACKAGE TCharacterisationForm *CharacterisationForm;
//---------------------------------------------------------------------------
#endif
