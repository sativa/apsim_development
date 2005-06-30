//---------------------------------------------------------------------------

#ifndef TStatsFormH
#define TStatsFormH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include "AdvGrid.hpp"
#include "BaseGrid.hpp"
#include <Grids.hpp>
//---------------------------------------------------------------------------
class TStatsForm : public TForm
{
__published:	// IDE-managed Components
   TAdvStringGrid *Grid;
   TButton *OkButton;
   TLabel *Label1;
   TLabel *OverallLabel;
private:	// User declarations
public:		// User declarations
   __fastcall TStatsForm(TComponent* Owner);

   
};
//---------------------------------------------------------------------------
extern PACKAGE TStatsForm *StatsForm;
//---------------------------------------------------------------------------
#endif
