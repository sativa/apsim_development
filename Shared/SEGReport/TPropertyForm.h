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
//---------------------------------------------------------------------------
class TPropertyForm : public TForm
{
__published:	// IDE-managed Components
   TComboBox *SourceCombo;
   TEdit *NameEdit;
   TLabel *SourceLabel;
   TLabel *Label2;
   TLabel *ToolbarLabel;
   TCheckBox *ToolbarCheckBox;
   void __fastcall NameEditExit(TObject *Sender);
   void __fastcall SourceComboChange(TObject *Sender);
   void __fastcall ToolbarCheckBoxClick(TObject *Sender);
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
