//---------------------------------------------------------------------------

#ifndef TChartFormH
#define TChartFormH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include "TGraph.h"
#include "TPropertyForm.h"
#include <Buttons.hpp>
//---------------------------------------------------------------------------
class TChartForm : public TPropertyForm
{
__published:	// IDE-managed Components
   TSpeedButton *ChartPropertiesButton;
   TEdit *SeriesNumberEdit;
   TLabel *Label3;
   void __fastcall ChartPropertiesButtonClick(TObject *Sender);
   void __fastcall SeriesNumberEditChange(TObject *Sender);
private:	// User declarations
   TGraph* graph;
public:		// User declarations
   __fastcall TChartForm(TComponent* Owner);

   virtual void setComponent(TComponent* component);

};
//---------------------------------------------------------------------------
extern PACKAGE TChartForm *ChartForm;
//---------------------------------------------------------------------------
#endif
