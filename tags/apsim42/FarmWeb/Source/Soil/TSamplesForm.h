//---------------------------------------------------------------------------

#ifndef TSamplesFormNewH
#define TSamplesFormNewH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include "AdvCGrid.hpp"
#include "AdvGrid.hpp"
#include "BaseGrid.hpp"
#include <ComCtrls.hpp>
#include <ExtCtrls.hpp>
#include <Grids.hpp>
#include "Soil.h"
#include "Soils.h"
#include "SoilSample.h"
#include <Menus.hpp>
#include <ActnList.hpp>
#include <Buttons.hpp>

//Samples Grid
static const unsigned SAMPLE_DATE = 0;
static const unsigned SAMPLE_NAME = 1;
static const unsigned SOIL_NAME = 2;

//Sample Grid
static const unsigned DEPTH = 0;
static const unsigned WET = 1;
static const unsigned DRY = 2;
static const unsigned NO3 = 3;
static const unsigned SW = 4;
static const unsigned NH4_COL = 5;
static const unsigned OC_COL = 6;
static const unsigned EC_COL = 7;
static const unsigned PH_COL = 8;
static const unsigned ESP_COL = 9;

//---------------------------------------------------------------------------
class __declspec(dllexport) TSamplesForm : public TForm
{
__published:	// IDE-managed Components
    TAdvColumnGrid *SelectorGrid;
    TPanel *Panel1;
    TAdvStringGrid *SampleGrid;
    TLabel *Label1;
    TDateTimePicker *SampleDateTimePicker;
    TLabel *Label2;
    TEdit *SampleNameEditBox;
    TLabel *Label3;
    TComboBox *SoilComboBox;
    TPopupMenu *PopupGridMenu;
    TMenuItem *NewSample1;
    TMenuItem *Cut1;
    TMenuItem *Copy1;
    TMenuItem *Paste1;
    TMenuItem *DeleteSample1;
    TActionList *ActionList1;
    TAction *NewSample;
    TAction *CutSample;
    TAction *PasteSample;
    TAction *CopySample;
    TAction *DeleteSample;
    TAction *ImportDepths;
    TSpeedButton *DeleteCropButton;
    void __fastcall FormResize(TObject *Sender);
    void __fastcall OnRowChange(TObject *Sender, int OldRow, int NewRow,
          bool &Allow);
    void __fastcall CanEditCell(TObject *Sender, int ARow, int ACol,
          bool &CanEdit);
    void __fastcall GridSetEditText(TObject *Sender, int ACol, int ARow,
          const AnsiString Value);
    void __fastcall OnSamplesSoilChange(TObject *Sender);
    void __fastcall NewSampleExecute(TObject *Sender);
    void __fastcall ImportDepthsExecute(TObject *Sender);
    void __fastcall CutSampleExecute(TObject *Sender);
    void __fastcall DeleteSampleExecute(TObject *Sender);
    void __fastcall CopySampleExecute(TObject *Sender);
    void __fastcall PasteSampleExecute(TObject *Sender);
    void __fastcall PopupGridMenuPopup(TObject *Sender);
    void __fastcall SelectorGridKeyDown(TObject *Sender, WORD &Key,
          TShiftState Shift);
private:	// User declarations
   Soils* soils;
   Soil* soil;
   String currentSoilName;
   String currentSampleName;
   int numbersamples;
   SoilSample* thesample;
   void loadGrids();
   bool dirtyData;
   bool readOnly;
   bool deleting;
public:		// User declarations
   void loadSample(const std::string& Soilname,const std::string& Samplename);
   void saveSample();
   void focusRowOfSoilNameandSample(const std::string& Soilname,const std::string& Samplename);
   std::string getCurrentSoilNameSelected(){return string(SelectorGrid->Cells[SOIL_NAME][SelectorGrid->Row].c_str());}
   std::string getCurrentSampleNameSelected(){return string(SelectorGrid->Cells[SAMPLE_NAME][SelectorGrid->Row].c_str());}
   void setup(const std::string& soilXML);
   void setup(Soils* soilsObject);
   bool isSampleInClipboard();
   bool colIsSWCol(int col);
   void calcAndDisplaySW();
   void enableSampleDisplay(){SampleGrid->Enabled = true;
                              SampleDateTimePicker->Enabled = true;
                              SampleNameEditBox->Enabled = true;
                              SoilComboBox->Enabled = true;
                              }
   void disableSampleDisplay(){SampleGrid->Enabled = false;
                               SampleDateTimePicker->Enabled = false;
                               SampleNameEditBox->Enabled = false;
                               SoilComboBox->Enabled = false;
                               ImportDepths->Enabled = false;
                              }
   bool isDirty();
   void save();
//   void setup(const std::string soilXML);
    __fastcall TSamplesForm(TComponent* Owner);
    __fastcall TSamplesForm(HWND Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TSamplesForm *SamplesForm;
//---------------------------------------------------------------------------
#endif
