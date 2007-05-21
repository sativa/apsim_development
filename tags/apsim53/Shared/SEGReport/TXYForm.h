//---------------------------------------------------------------------------

#ifndef TXYFormH
#define TXYFormH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include "AdvCGrid.hpp"
#include "AdvGrid.hpp"
#include "AdvPanel.hpp"
#include "BaseGrid.hpp"
#include "TPropertyForm.h"
#include <ExtCtrls.hpp>
#include <Grids.hpp>
#include <ImgList.hpp>
#include "TXYGraph.h"

//---------------------------------------------------------------------------
class TXYForm : public TPropertyForm
   {
   __published:	// IDE-managed Components
      TAdvColumnGrid *YVariableList;
      TLabel *Label1;
      TComboBox *XVariableList;
      TLabel *Label4;
      TImageList *TypeImages;
   TLabel *ChartPropertyLabel;
   TLabel *CumLabel;
   TCheckBox *CumX;
      void __fastcall YVariableListComboCloseUp(TObject *Sender, int ARow,
          int ACol);
      void __fastcall YVariableListEditingDone(TObject *Sender);
   void __fastcall YVariableListCheckBoxClick(TObject *Sender, int ACol,
          int ARow, bool State);
   void __fastcall ChartPropertyLabelClick(TObject *Sender);
   void __fastcall XVariableListChange(TObject *Sender);
   void __fastcall CumXClick(TObject *Sender);
   private:	// User declarations
      TXYGraph* graph;

      void populateGraph(void);

   public:		// User declarations
      __fastcall TXYForm(TComponent* Owner);

      void setComponent(TComponent* component);

   };
//---------------------------------------------------------------------------
extern PACKAGE TXYForm *XYForm;
//---------------------------------------------------------------------------
#endif
