//---------------------------------------------------------------------------

#ifndef TChartFormH
#define TChartFormH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include <Buttons.hpp>
#include <ComCtrls.hpp>
#include "TSEGChart.h"
//---------------------------------------------------------------------------
class TChartForm : public TForm
{
__published:	// IDE-managed Components
   TPageControl *PageControl1;
   TTabSheet *PropertiesSheet;
   TSpeedButton *ChartPropertiesButton;
   TTabSheet *AdvancedSheet;
   TLabel *Label2;
   TEdit *NameEdit;
   TCheckBox *DuplicateCheckBox;
   TLabel *Label1;
   void __fastcall NameEditExit(TObject *Sender);
   void __fastcall ChartPropertiesButtonClick(TObject *Sender);
   void __fastcall DuplicateCheckBoxClick(TObject *Sender);
private:	// User declarations
   TSEGChart* chart;
public:		// User declarations
   __fastcall TChartForm(TComponent* Owner);
   void setComponent(TSEGChart* chart);
};
//---------------------------------------------------------------------------
extern PACKAGE TChartForm *ChartForm;
//---------------------------------------------------------------------------
#endif
