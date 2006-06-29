//---------------------------------------------------------------------------

#ifndef TSOIFormH
#define TSOIFormH
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
#include "TSOI.h"
#include <DB.hpp>
#include "DBAdvGrd.hpp"
#include "AdvPanel.hpp"
#include <ExtCtrls.hpp>
//---------------------------------------------------------------------------
class TSOIForm : public TPropertyForm
{
__published:	// IDE-managed Components
   TCheckBox *ZeroCheckBox;
   TCheckBox *RisingCheckBox;
   TCheckBox *FallingCheckBox;
   TCheckBox *PositiveCheckBox;
   TCheckBox *NegativeCheckBox;
   TComboBox *MonthCombo;
   TLabel *Label8;
   TCheckBox *GetFromSourceCheckBox;
   TLabel *Label1;
   TLabel *Label3;
   TCheckBox *AllOtherYearsCheckBox;
   void __fastcall NegativeCheckBoxClick(TObject *Sender);
   void __fastcall PositiveCheckBoxClick(TObject *Sender);
   void __fastcall FallingCheckBoxClick(TObject *Sender);
   void __fastcall RisingCheckBoxClick(TObject *Sender);
   void __fastcall ZeroCheckBoxClick(TObject *Sender);
   void __fastcall MonthComboClick(TObject *Sender);
   void __fastcall GetFromSourceCheckBoxClick(TObject *Sender);
   void __fastcall AllOtherYearsCheckBoxClick(TObject *Sender);
private:	// User declarations
   TSOI* soi;
public:		// User declarations
   __fastcall TSOIForm(TComponent* Owner);
   virtual void setComponent(TComponent* soi);
};
//---------------------------------------------------------------------------
extern PACKAGE TSOIForm *SOIForm;
//---------------------------------------------------------------------------
#endif
