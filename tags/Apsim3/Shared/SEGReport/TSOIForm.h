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
//---------------------------------------------------------------------------
class TSOIForm : public TSEGTableForm
{
__published:	// IDE-managed Components
   TLabel *Label3;
   TListBox *SOIList;
   void __fastcall SOIListClick(TObject *Sender);
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
