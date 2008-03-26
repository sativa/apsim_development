//---------------------------------------------------------------------------

#ifndef TXY_formH
#define TXY_formH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include "TAnalysis_form.h"
#include <Buttons.hpp>
#include "AdvGrid.hpp"
#include <ComCtrls.hpp>
#include <Grids.hpp>
#include <ImgList.hpp>
#include "AsgLinks.hpp"
#include "BaseGrid.hpp"
#include <ExtCtrls.hpp>
#include "TXY_analysis.h"
//---------------------------------------------------------------------------
class TXY_form : public TAnalysis_form
{
__published:	// IDE-managed Components
   TImagePickerEditLink *ImagePickerEditLink;
   TPageControl *PageControl1;
   TTabSheet *TabSheet1;
   TTabSheet *TabSheet2;
   TLabel *Label4;
   TComboBox *XVariableList;
   TLabel *Label3;
   TAdvStringGrid *YVariableList;
   TAdvStringGrid *ScenarioGrid;
   TImageList *ImageList2;
   TCheckEditLink *CheckEditLink;
   void __fastcall FormShow(TObject *Sender);
   void __fastcall FormClose(TObject *Sender, TCloseAction &Action);
   void __fastcall YVariableListGetEditorType(TObject *Sender, int ACol,
          int ARow, TEditorType &AEditor);
   void __fastcall YVariableListGetEditorProp(TObject *Sender, int ACol,
          int ARow, TEditLink *AEditLink);
   void __fastcall YVariableListCheckBoxClick(TObject *Sender, int ACol,
          int ARow, bool State);
   void __fastcall ScenarioGridGetEditorType(TObject *Sender, int ACol,
          int ARow, TEditorType &AEditor);
   void __fastcall ScenarioGridGetEditorProp(TObject *Sender, int ACol,
          int ARow, TEditLink *AEditLink);
   void __fastcall XVariableListChange(TObject *Sender);
private:	// User declarations
   virtual bool allowEnableOkButton(void);
   void fillScenarioGrid(void);
   TXY_analysis::StatType textToStatType(const std::string& text);
public:		// User declarations
   __fastcall TXY_form(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TXY_form *XY_form;
//---------------------------------------------------------------------------
#endif
