//---------------------------------------------------------------------------

#ifndef TRecFilterFormH
#define TRecFilterFormH
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
#include "TRecFilter.h"
#include <DB.hpp>
#include "DBAdvGrd.hpp"
#include "AdvPanel.hpp"
#include <ExtCtrls.hpp>
//---------------------------------------------------------------------------
class TRecFilterForm : public TPropertyForm
   {
   __published:	// IDE-managed Components
      TEdit *RecEdit;
     TLabel *RecLabel;
     TCheckBox *FirstCheckBox;
     TCheckBox *LastCheckBox;
     void __fastcall RecEditExit(TObject *Sender);
   void __fastcall FirstCheckBoxClick(TObject *Sender);
   void __fastcall LastCheckBoxClick(TObject *Sender);
   private:	// User declarations
      TRecFilter* recFilter;
   public:		// User declarations
      __fastcall TRecFilterForm(TComponent* Owner);
      void setComponent(TComponent* component);
   };
//---------------------------------------------------------------------------
extern PACKAGE TRecFilterForm *RecFilterForm;
//---------------------------------------------------------------------------
#endif
