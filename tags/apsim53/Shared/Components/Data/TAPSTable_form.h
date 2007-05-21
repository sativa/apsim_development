//---------------------------------------------------------------------------
#ifndef TAPSTable_formH
#define TAPSTable_formH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include <Buttons.hpp>
#include "TAPSTable.h"
#include <ComCtrls.hpp>
//---------------------------------------------------------------------------
class TAPSTable_form : public TForm
{
__published:	// IDE-managed Components
   TBitBtn *OkButton;
   TBitBtn *BitBtn2;
private:	// User declarations

public:		// User declarations
   __fastcall TAPSTable_form(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TAPSTable_form *APSTable_form;
//---------------------------------------------------------------------------
#endif
