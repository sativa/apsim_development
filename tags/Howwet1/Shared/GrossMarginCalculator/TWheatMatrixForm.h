//---------------------------------------------------------------------------

#ifndef TWheatMatrixFormH
#define TWheatMatrixFormH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include <DBGrids.hpp>
#include <Grids.hpp>
#include <ExtCtrls.hpp>
#include <DBCtrls.hpp>
#include "AdvCGrid.hpp"
#include "AdvGrid.hpp"
#include "BaseGrid.hpp"
#include <GrossMarginCalculator\GMCalculator.h>
//---------------------------------------------------------------------------
class TWheatMatrixForm : public TForm
   {
   __published:	// IDE-managed Components
   TAdvColumnGrid *ProteinIncrementGrid;
   TLabel *Label3;
   TButton *Button1;
   TButton *Button2;
   private:	// User declarations
   public:		// User declarations
      __fastcall TWheatMatrixForm(TComponent* Owner);
      GMCalculator* data;
   };
//---------------------------------------------------------------------------
extern PACKAGE TWheatMatrixForm *WheatMatrixForm;
//---------------------------------------------------------------------------
#endif
