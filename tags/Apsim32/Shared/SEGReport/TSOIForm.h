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
//---------------------------------------------------------------------------
class TSOIForm : public TPropertyForm
{
__published:	// IDE-managed Components
   TComboBox *MonthCombo;
   TLabel *Label3;
   TLabel *Label4;
   TLabel *Label5;
   TLabel *Label6;
   TLabel *Label7;
   TCheckBox *NegativeCheckBox;
   TCheckBox *PositiveCheckBox;
   TCheckBox *FallingCheckBox;
   TCheckBox *RisingCheckBox;
   TCheckBox *ZeroCheckBox;
   TLabel *Label8;
   void __fastcall NegativeCheckBoxClick(TObject *Sender);
   void __fastcall PositiveCheckBoxClick(TObject *Sender);
   void __fastcall FallingCheckBoxClick(TObject *Sender);
   void __fastcall RisingCheckBoxClick(TObject *Sender);
   void __fastcall ZeroCheckBoxClick(TObject *Sender);
   void __fastcall MonthComboClick(TObject *Sender);
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
