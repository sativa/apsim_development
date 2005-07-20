//---------------------------------------------------------------------------

#ifndef TSoilPickerFormH
#define TSoilPickerFormH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include <ExtCtrls.hpp>
#include "Soils.h"
#include "AdvCGrid.hpp"
#include "AdvGrid.hpp"
#include "BaseGrid.hpp"
#include <Grids.hpp>
#include <Dialogs.hpp>
#include <ActnList.hpp>
#include <Menus.hpp>
class TCharacterisationForm;
//---------------------------------------------------------------------------
class __declspec(dllexport)TSoilPickerForm: public TForm
   {
   __published:	// IDE-managed Components
   TPanel *ButtonPanel;
   TButton *OkButton;
   TButton *CancelButton;
   TPanel *Panel1;
   TAdvColumnGrid *SelectorGrid;
   TPanel *SoilDetails;
   TSplitter *Splitter1;
   TButton *BrowseButton;
   TOpenDialog *OpenDialog;
    TPopupMenu *PopupGridMenu;
    TMenuItem *New1;
    TActionList *ActionList1;
    TAction *InsertNewSoil;
    TAction *CutAction;
    TAction *PasteAction;
    TAction *CopyAction;
    TAction *DeleteAction;
    TMenuItem *Cut1;
    TMenuItem *Copy1;
    TMenuItem *Paste1;
    TMenuItem *Delete1;
    void __fastcall OnRowChange(TObject *Sender, int OldRow, int NewRow,
          bool &Allow);
    void __fastcall SelectorGridKeyDown(TObject *Sender, WORD &Key,
          TShiftState Shift);
   void __fastcall FormResize(TObject *Sender);
   void __fastcall BrowseButtonClick(TObject *Sender);
    void __fastcall InsertNewSoilExecute(TObject *Sender);
    void __fastcall CutActionExecute(TObject *Sender);
    void __fastcall CopyActionExecute(TObject *Sender);
    void __fastcall PasteActionExecute(TObject *Sender);
    void __fastcall DeleteActionExecute(TObject *Sender);
    void __fastcall PopupGridMenuPopup(TObject *Sender);

   private:	// User declarations
      TCharacterisationForm* form;
      bool dirtyData;
      std::string defaultSoilName;
      
   public:		// User declarations
      __fastcall TSoilPickerForm(TComponent* Owner);
      __fastcall TSoilPickerForm(HWND Owner);
    //  void setup(Soils& soils, bool readOnly, unsigned refno);
      void setup(const std::string& fileName,  const std::string& defaultSoilName,
                 bool readOnly, bool showOkCancel);
      void setup(Soils* fileName,  const std::string& defaultSoilName,
                 bool readOnly, bool showOkCancel);
      void focusRowOfSoilName(const std::string& name);
      void setSoil(const std::string& name);
      bool isDirty();
      void deleteSoil();
      void importSoil(const std::string& name);
      void importPar(const std::string& name);
      void importXls(const std::string& name);
      void importSoils(const std::string& name);
      void importW2N2(const std::string& directoryPath);
      void saveSoils(std::string filename);
      bool isSoilInClipboard();
      void pasteSoil();
      void copySoil();
      void cutSoil();
      std::string getCurrentSoilNameSelected();
      std::string getCurrentSoilNameDisplayed();
      std::string getCurrentDbFileName(void) {return soils->getFileName();}
      void saveCurrentSoil(void);      
   private:
      std::string CurrentSoilName;
      Soils* soils;
      Soil* soil;
      bool readonly;
      void loadGrids(void);
      void reloadGrids(int UpdateRow);

   };
//---------------------------------------------------------------------------
extern PACKAGE TSoilPickerForm *SoilPickerForm;
//---------------------------------------------------------------------------
#endif
