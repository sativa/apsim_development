//---------------------------------------------------------------------------

#ifndef TRegrFormH
#define TRegrFormH
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
#include "TRegr.h"
#include <DB.hpp>
#include "AdvPanel.hpp"
#include <ExtCtrls.hpp>
#include "TPropertyForm.h"
//---------------------------------------------------------------------------
class TRegrForm : public TPropertyForm
{
__published:	// IDE-managed Components
   TComboBox *XCombo;
   TLabel *Label3;
   TComboBox *YCombo;
   TLabel *Label1;
   void __fastcall XComboChange(TObject *Sender);
   void __fastcall YComboChange(TObject *Sender);
private:	// User declarations
   TRegr* regr;
public:		// User declarations
   __fastcall TRegrForm(TComponent* Owner);
   virtual void setComponent(TComponent* component);
};
//---------------------------------------------------------------------------
extern PACKAGE TRegrForm *RegrForm;
//---------------------------------------------------------------------------
#endif
