//---------------------------------------------------------------------------

#ifndef TFrequencyFormH
#define TFrequencyFormH
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
#include "TFrequency.h"
#include <DB.hpp>
#include "AdvPanel.hpp"
#include <ExtCtrls.hpp>
//---------------------------------------------------------------------------
class TFrequencyForm : public TPropertyForm
   {
   __published:	// IDE-managed Components
   TStringGrid *Grid;
   void __fastcall GridEditingDone(TObject *Sender);
   private:	// User declarations
      TFrequency* frequency;
   public:		// User declarations
      __fastcall TFrequencyForm(TComponent* Owner);
      virtual void setComponent(TComponent* component);
   };
//---------------------------------------------------------------------------
extern PACKAGE TFrequencyForm *FrequencyForm;
//---------------------------------------------------------------------------
#endif
