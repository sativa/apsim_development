//---------------------------------------------------------------------------

#ifndef TChartLineFormH
#define TChartLineFormH
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
#include "TChartLine.h"
#include <DB.hpp>
#include "AdvPanel.hpp"
#include <ExtCtrls.hpp>
//---------------------------------------------------------------------------
class TChartLineForm : public TPropertyForm
   {
   __published:	// IDE-managed Components
      TComboBox *FieldNameCombo;
      TLabel *Label3;
   TLabel *Label1;
   TEdit *X1Edit;
   TLabel *Label4;
   TEdit *X2Edit;
   TLabel *Label5;
   TEdit *LabelEdit;
      void __fastcall FieldNameComboChange(TObject *Sender);
   void __fastcall X1EditChange(TObject *Sender);
   void __fastcall X2EditChange(TObject *Sender);
   void __fastcall LabelEditExit(TObject *Sender);
   private:	// User declarations
      TChartLine* chartLine;
   public:		// User declarations
      __fastcall TChartLineForm(TComponent* Owner);
      virtual void setComponent(TComponent* component);
   };
//---------------------------------------------------------------------------
extern PACKAGE TChartLineForm *ChartLineForm;
//---------------------------------------------------------------------------
#endif
