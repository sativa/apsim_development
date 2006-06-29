//---------------------------------------------------------------------------

#ifndef TKWTestFormH
#define TKWTestFormH
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
#include "TKWTest.h"
#include <DB.hpp>
#include "AdvPanel.hpp"
#include <ExtCtrls.hpp>
//---------------------------------------------------------------------------
class TKWTestForm : public TPropertyForm
{
__published:	// IDE-managed Components
   TComboBox *FieldNameCombo;
   TLabel *Label3;
   TLabel *Label1;
   TComboBox *Source2Combo;
   void __fastcall FieldNameComboChange(TObject *Sender);
   void __fastcall Source2ComboChange(TObject *Sender);
private:	// User declarations
   TKWTest* kwTest;
public:		// User declarations
   __fastcall TKWTestForm(TComponent* Owner);
   virtual void setComponent(TComponent* component);
};
//---------------------------------------------------------------------------
extern PACKAGE TKWTestForm *KWTestForm;
//---------------------------------------------------------------------------
#endif
