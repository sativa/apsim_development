//---------------------------------------------------------------------------

#ifndef TPropertyFormH
#define TPropertyFormH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include "AdvGrid.hpp"
#include "BaseGrid.hpp"
#include "dbadvgrd.hpp"
#include <ComCtrls.hpp>
#include <Db.hpp>
#include <Grids.hpp>
#include "TSEGTable.h"
#include <DB.hpp>
#include "DBAdvGrd.hpp"
#include "AdvPanel.hpp"
#include <ExtCtrls.hpp>
//---------------------------------------------------------------------------
class TPropertyForm : public TForm
{
__published:	// IDE-managed Components
   TAdvPanelGroup *AdvPanelGroup1;
   TAdvPanel *AdvancedPanel;
   TLabel *Label2;
   TEdit *NameEdit;
   TLabel *SourceLabel;
   TComboBox *SourceCombo;
   TCheckBox *ToolbarCheckBox;
   TLabel *SortFieldsLabel;
   TEdit *SortFieldsEdit;
   TLabel *PivotLabel;
   TEdit *GroupByEdit;
   TAdvPanel *PropertyPanel;
   void __fastcall NameEditExit(TObject *Sender);
   void __fastcall SourceComboChange(TObject *Sender);
   void __fastcall ToolbarCheckBoxClick(TObject *Sender);
   void __fastcall SortFieldsEditChange(TObject *Sender);
   void __fastcall GroupByEditExit(TObject *Sender);
private:	// User declarations
   TComponent* component;
protected:
   virtual void sourceHasChanged(TSEGTable* segTable) { };
public:		// User declarations
   __fastcall TPropertyForm(TComponent* Owner);
   __fastcall ~TPropertyForm(void);
   virtual void setComponent(TComponent* component);
};
//---------------------------------------------------------------------------
extern PACKAGE TPropertyForm *PropertyForm;
//---------------------------------------------------------------------------
#endif
