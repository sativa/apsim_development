//---------------------------------------------------------------------------
#ifndef TPie_frequency_formH
#define TPie_frequency_formH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include "TAnalysis_form.h"
#include <Buttons.hpp>
#include <Grids.hpp>
#include <ComCtrls.hpp>
#include <ImgList.hpp>
//---------------------------------------------------------------------------
class TPie_frequency_form : public TAnalysis_form
{
__published:	// IDE-managed Components
   TEdit *Num_percentiles_edit;
   TLabel *Label3;
   TStringGrid *Percentile_grid;
   TLabel *Label4;
   TComboBox *BaseDataset;
   TComboBox *SecondDataset;
   TLabel *Label5;
   TLabel *Label6;
   void __fastcall FormShow(TObject *Sender);
   void __fastcall FormClose(TObject *Sender, TCloseAction &Action);
   void __fastcall Num_percentiles_editChange(TObject *Sender);
private:	// User declarations
public:		// User declarations
   __fastcall TPie_frequency_form(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TPie_frequency_form *Percentile_frequency_form;
//---------------------------------------------------------------------------
#endif
