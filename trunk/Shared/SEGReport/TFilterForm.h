//---------------------------------------------------------------------------

#ifndef TFilterFormH
#define TFilterFormH
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
#include "TFilter.h"
#include <DB.hpp>
#include "DBAdvGrd.hpp"
//---------------------------------------------------------------------------
class TFilterForm : public TSEGTableForm
   {
   __published:	// IDE-managed Components
      TLabel *Label3;
      TComboBox *FieldNameCombo;
      TLabel *Label4;
      TComboBox *FieldValueCombo;
      void __fastcall FieldNameComboChange(TObject *Sender);
      void __fastcall FieldValueComboChange(TObject *Sender);
   void __fastcall PropertiesSheetShow(TObject *Sender);
   private:	// User declarations
      ::TFilter* filter;
   public:		// User declarations
      __fastcall TFilterForm(TComponent* Owner);
      void setComponent(::TFilter* filter);
   };
//---------------------------------------------------------------------------
extern PACKAGE TFilterForm *FilterForm;
//---------------------------------------------------------------------------
#endif
