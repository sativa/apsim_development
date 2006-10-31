//---------------------------------------------------------------------------
#ifndef TAnalysis_formH
#define TAnalysis_formH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include "TAPSTable_form.h"
#include <Buttons.hpp>
#include <ComCtrls.hpp>
#include <ImgList.hpp>
class TAnalysis;
//---------------------------------------------------------------------------
class TAnalysis_form : public TAPSTable_form
{
__published:	// IDE-managed Components
   TLabel *Label2;
   TLabel *Label1;
   TListView *VariableList;
   TImageList *ImageList1;
   TTreeView *ScenarioTree;
   TLabel *ExpandLabel;
   TLabel *CollapseLabel;
   void __fastcall FormShow(TObject *Sender);
   void __fastcall FormClose(TObject *Sender, TCloseAction &Action);
//   void __fastcall ScenarioTreeChecked(TObject *Sender, TTreeNTNode *Node);
   void __fastcall VariableListChange(TObject *Sender, TListItem *Item,
          TItemChange Change);
   void __fastcall ScenarioTreeClick(TObject *Sender);
   void __fastcall ExpandLabelClick(TObject *Sender);
   void __fastcall CollapseLabelClick(TObject *Sender);
   void __fastcall ScenarioTreeExpanding(TObject *Sender, TTreeNode *Node,
          bool &AllowExpansion);
   void __fastcall ScenarioTreeCollapsing(TObject *Sender, TTreeNode *Node,
          bool &AllowCollapse);
private:	// User declarations
   bool multipleVariables;
   bool weAreExpanding;
   void         showScenarios(void);
   void         showVariables(void);
   void         saveScenarios(void);
   void         saveVariables(void);
   TTreeNode* findNodeInTree(TTreeNode* parentNode, AnsiString& name);
   void         expandAllNodes(bool expand);
   void         toggleImageOnNode(TTreeNode* node);
   void         recursiveToggleImageOnNode(TTreeNode* node);  
protected:
   virtual bool allowEnableOkButton(void) {return true;}
   void         checkOkButton(void);

public:		// User declarations
   __fastcall TAnalysis_form(TComponent* Owner);
      TAnalysis* Analysis_ptr;
   void allowMultipleVariables(void) {multipleVariables = true;}
};
//---------------------------------------------------------------------------
extern PACKAGE TAnalysis_form *Analysis_form;
//---------------------------------------------------------------------------
#endif
