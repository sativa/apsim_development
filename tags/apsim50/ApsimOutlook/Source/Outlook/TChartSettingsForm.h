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
   void __fastcall OnMinimize(TMessage &Msg);

   BEGIN_MESSAGE_MAP
       MESSAGE_HANDLER(WM_SYSCOMMAND, TMessage, OnMinimize)
   END_MESSAGE_MAP(TForm)
public:		// User declarations
   __fastcall TChartSettingsForm(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TChartSettingsForm *ChartSettingsForm;
//---------------------------------------------------------------------------
#endif
