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
#include "TSEGTableForm.h"
#include <ComCtrls.hpp>
#include <Db.hpp>
#include <Grids.hpp>
#include "TSOI.h"
#include <DB.hpp>
#include "DBAdvGrd.hpp"
//---------------------------------------------------------------------------
class TSOIForm : public TSEGTableForm
{
__published:	// IDE-managed Components
   TLabel *Label3;
   TListBox *SOIList;
   TCheckBox *NegativeCheckBox;
   TCheckBox *PositiveCheckBox;
   TCheckBox *FallingCheckBox;
   TCheckBox *RisingCheckBox;
   TCheckBox *ZeroCheckBox;
   void __fastcall SOIListClick(TObject *Sender);
   void __fastcall NegativeCheckBoxClick(TObject *Sender);
   void __fastcall PositiveCheckBoxClick(TObject *Sender);
   void __fastcall FallingCheckBoxClick(TObject *Sender);
   void __fastcall RisingCheckBoxClick(TObject *Sender);
   void __fastcall ZeroCheckBoxClick(TObject *Sender);
private:	// User declarations
   TSOI* soi;
public:		// User declarations
   __fastcall TSOIForm(TComponent* Owner);
   void setComponent(TSOI* soi);
};
//---------------------------------------------------------------------------
extern PACKAGE TSOIForm *SOIForm;
//---------------------------------------------------------------------------
#endif
