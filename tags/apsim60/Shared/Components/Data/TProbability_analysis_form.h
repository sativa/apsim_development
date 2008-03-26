//---------------------------------------------------------------------------
#ifndef TProbability_analysis_formH
#define TProbability_analysis_formH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include "TAnalysis_form.h"
#include <Buttons.hpp>
#include <ComCtrls.hpp>
#include "paramtreeview.hpp"
#include <ImgList.hpp>
//---------------------------------------------------------------------------
class TProbability_analysis_form : public TAnalysis_form
{
__published:	// IDE-managed Components
   TRadioButton *Probability_exceedence_radio;
   TRadioButton *Cumulative_probability_radio;
   void __fastcall FormShow(TObject *Sender);
   void __fastcall FormClose(TObject *Sender, TCloseAction &Action);
private:	// User declarations
public:		// User declarations
   __fastcall TProbability_analysis_form(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TProbability_analysis_form *Probability_analysis_form;
//---------------------------------------------------------------------------
#endif
