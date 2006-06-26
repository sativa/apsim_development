//---------------------------------------------------------------------------

#ifndef TDiffFormH
#define TDiffFormH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include "AdvGrid.hpp"
#include "BaseGrid.hpp"
#include "dbadvgrd.hpp"
#include "TPropertyForm.h"
#include <ComCtrls.hpp>
#include <Db.hpp>
#include <Grids.hpp>
#include "TDiff.h"
#include <DB.hpp>
#include "DBAdvGrd.hpp"
#include "AdvPanel.hpp"
#include <ExtCtrls.hpp>
//---------------------------------------------------------------------------
class TDiffForm : public TPropertyForm
{
__published:	// IDE-managed Components
   TLabel *Label3;
   TComboBox *FieldNameCombo;
   TComboBox *SecondDataSetCombo;
   TLabel *Label1;
   void __fastcall FieldNameComboChange(TObject *Sender);
   void __fastcall SecondDataSetComboChange(TObject *Sender);
private:	// User declarations
   TDiff* diff;
   virtual void sourceHasChanged(TSEGTable* segTable);   
public:		// User declarations
   __fastcall TDiffForm(TComponent* Owner);
   void setComponent(TComponent* component);
};
//---------------------------------------------------------------------------
extern PACKAGE TDiffForm *DiffForm;
//---------------------------------------------------------------------------
#endif
