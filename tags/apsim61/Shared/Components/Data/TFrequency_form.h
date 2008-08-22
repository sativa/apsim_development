//---------------------------------------------------------------------------
#ifndef TFrequency_formH
#define TFrequency_formH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include "TAnalysis_form.h"
#include <Buttons.hpp>
#include <ComCtrls.hpp>
#include "AdvSpin.hpp"
#include <Mask.hpp>
#include "paramtreeview.hpp"
#include <ImgList.hpp>
//---------------------------------------------------------------------------
class TFrequency_form : public TAnalysis_form
{
__published:	// IDE-managed Components
   TLabel *Num_frequencies_label;
   TGroupBox *GroupBox1;
   TRadioButton *Frequency_radio;
   TRadioButton *Probability_radio;
   TAdvSpinEdit *Num_frequencies_spin;
   void __fastcall FormShow(TObject *Sender);
   void __fastcall FormClose(TObject *Sender, TCloseAction &Action);
private:	// User declarations
public:		// User declarations
   __fastcall TFrequency_form(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TFrequency_form *Frequency_form;
//---------------------------------------------------------------------------
#endif
