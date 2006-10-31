//---------------------------------------------------------------------------

#ifndef TSeedWeightFOrmH
#define TSeedWeightFOrmH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include <DBGrids.hpp>
#include <ExtCtrls.hpp>
#include <Grids.hpp>
#include "AdvCGrid.hpp"
#include "AdvGrid.hpp"
#include "BaseGrid.hpp"
//---------------------------------------------------------------------------
class TSeedWeightsForm : public TForm
   {
   __published:	// IDE-managed Components
   TAdvColumnGrid *SeedWeightGrid;
   TLabel *Label1;
   TButton *Button1;
   TButton *Button2;
   private:	// User declarations
   public:		// User declarations
      __fastcall TSeedWeightsForm(TComponent* Owner);
   };
//---------------------------------------------------------------------------
extern PACKAGE TSeedWeightsForm *SeedWeightsForm;
//---------------------------------------------------------------------------
#endif
