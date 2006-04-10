//---------------------------------------------------------------------------
#ifndef TTime_series_formH
#define TTime_series_formH
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
class TTime_series_form : public TAnalysis_form
{
__published:	// IDE-managed Components
   TRadioButton *Time_series_radio;
   TRadioButton *Diff_from_mean_radio;
   TRadioButton *Diff_from_percentile_radio;
   TEdit *Percentile_edit;
   TComboBox *Base_dataset_combo;
   TLabel *Base_dataset_label;
   TLabel *Percentile_label;
   TRadioButton *Time_series_mean_radio;
   TRadioButton *Time_series_percentile_radio;
   TCheckBox *Different_colours_checkbox;
   void __fastcall FormShow(TObject *Sender);
   void __fastcall FormClose(TObject *Sender, TCloseAction &Action);
   void __fastcall Enable_percentile(TObject *Sender);
private:	// User declarations
   virtual bool allowEnableOkButton(void);
   void fillBaseDataSetCombo(void);

public:		// User declarations
   __fastcall TTime_series_form(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TTime_series_form *Time_series_form;
//---------------------------------------------------------------------------
#endif
  