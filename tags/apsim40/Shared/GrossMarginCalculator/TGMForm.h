//---------------------------------------------------------------------------

#ifndef TGMFormH
#define TGMFormH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include <Db.hpp>
#include <ExtCtrls.hpp>
#include <Buttons.hpp>
#include <ComCtrls.hpp>
#include <DBCtrls.hpp>
#include <Mask.hpp>
#include <DBGrids.hpp>
#include <Grids.hpp>
#include <Menus.hpp>
#include <ImgList.hpp>
#include <ActnList.hpp>
#include <StdActns.hpp>
#include <ToolWin.hpp>
#include <Dialogs.hpp>
#include "AdvGrid.hpp"
#include "BaseGrid.hpp"
#include "AdvCGrid.hpp"
#include "AdvEdit.hpp"
#include <GrossMarginCalculator\GMCalculator.h>
#include "AdvCGrid.hpp"
//---------------------------------------------------------------------------
class TGMForm : public TForm
   {
   __published:	// IDE-managed Components
      TPageControl *GMInfo;
      TTabSheet *ScenarioPage;
      TTabSheet *CropCostPage;
      TLabel *Label2;
      TTabSheet *CropPricePage;
      TDBMemo *Notes;
      TLabel *Label12;
      TTreeView *ScenarioTree;
      TLabel *ScenarioLabel;
      TLabel *Label22;
      TAdvColumnGrid *CostGrid;
      TImageList *ImageList1;
      TCoolBar *CoolBar1;
      TToolBar *ToolBar1;
      TToolButton *OpenButton;
      TToolButton *ToolButton2;
      TToolButton *AddButton;
      TToolButton *DeleteButton;
      TToolButton *RenameButton;
      TToolButton *ToolButton3;
      TToolButton *CopyButton;
      TToolButton *PasteButton;
      TToolButton *PrimeHardButton;
      TToolButton *ToolButton7;
      TToolButton *SeedWeightButton;
      TLabel *Label26;
      TButton *OkButton;
      TLabel *PriceLabel;
      TLabel *Label3;
      TLabel *Label4;
      TLabel *Label5;
      TLabel *Label6;
      TLabel *Label7;
      TLabel *Label8;
      TLabel *Label9;
      TLabel *Label10;
      TLabel *Label11;
      TLabel *Label13;
      TAdvEdit *PriceEdit;
      TLabel *Label14;
      TLabel *Label15;
      TLabel *Label16;
      TLabel *Label17;
      TAdvEdit *DowngradeReturnEdit;
      TAdvEdit *HarvestLossEdit;
      TAdvEdit *MoistureContentEdit;
      TAdvEdit *DowngradePercentEdit;
      TLabel *Label18;
   TToolButton *NewButton;
      TOpenDialog *OpenDialog;
      TButton *CancelButton;
   TSaveDialog *SaveDialog;
   TPopupMenu *AddPopup;
   TMenuItem *AddScenario1;
   TMenuItem *AddCrop1;
   TPopupMenu *CostsPopup;
   TMenuItem *Deleterow1;
      void __fastcall FormShow(TObject *Sender);
      void __fastcall ScenarioTreeChange(TObject *Sender, TTreeNode *Node);
      void __fastcall CropPricePageShow(TObject *Sender);
      void __fastcall addScenario(TObject *Sender);
      void __fastcall addCrop(TObject *Sender);
      void __fastcall copySelected(TObject *Sender);
      void __fastcall pasteSelected(TObject *Sender);
      void __fastcall deleteSelected(TObject* sender);
      void __fastcall renameSelected(TObject* sender);
      void __fastcall ScenarioTreeEdited(TObject *Sender, TTreeNode *Node,
          AnsiString &S);
      void __fastcall ScenarioTreeEditing(TObject *Sender, TTreeNode *Node,
          bool &AllowEdit);
      void __fastcall WheatProteinButtonClick(TObject *Sender);
      void __fastcall SeedWeightButtonClick(TObject *Sender);
      void __fastcall CropCostPageShow(TObject *Sender);
      void __fastcall CostGridGetEditorType(TObject *Sender, int ACol,
             int ARow, TEditorType &AEditor);
      void __fastcall DowngradePercentEditChange(TObject *Sender);
      void __fastcall PriceEditBoxesChanged(TObject *Sender);
      void __fastcall ScenarioTreeChanging(TObject *Sender, TTreeNode *Node,
             bool &AllowChange);
      void __fastcall GMInfoChanging(TObject *Sender, bool &AllowChange);
      void __fastcall CostGridCellsChanged(TObject *Sender, TRect &R);
      void __fastcall OpenButtonClick(TObject *Sender);
      void __fastcall FormClose(TObject *Sender, TCloseAction &Action);
   void __fastcall NewButtonClick(TObject *Sender);
   void __fastcall Deleterow1Click(TObject *Sender);

   private:	// User declarations
      GMCalculator* data;
      bool dataNeedsSaving;
      std::string tempFilename;
      std::string copiedScenarioName;

      struct CropData
         {
         std::string cropName;
         vector<GMCalculator::Costs> costs;
         GMCalculator::Price price;
         };
      std::vector<CropData> copiedCrops;

      void openDatabase(const std::string& fileName);
      void populateTree(void);
      void updateLabels(void);
      AnsiString newName(AnsiString oldName);
      bool scenarioExists(AnsiString oldName);
      void saveDataIfNecessary(void);
      void populateCostPage(void);
      void saveCostPage(void);
      void populatePricePage(void);
      void savePricePage(void);

   public:		// User declarations
      __fastcall TGMForm(TComponent* Owner);
      __fastcall ~TGMForm();

      std::string mdbFileName;
   };
//---------------------------------------------------------------------------
extern PACKAGE TGMForm *GMForm;
//---------------------------------------------------------------------------
#endif
