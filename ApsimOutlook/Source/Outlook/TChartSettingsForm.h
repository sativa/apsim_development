//---------------------------------------------------------------------------
#ifndef TChartSettingsFormH
#define TChartSettingsFormH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
//---------------------------------------------------------------------------
class TChartSettingsForm : public TForm
{
__published:	// IDE-managed Components
   TMemo *Settings_list;
private:	// User declarations
public:		// User declarations
   __fastcall TChartSettingsForm(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TChartSettingsForm *ChartSettingsForm;
//---------------------------------------------------------------------------
#endif
