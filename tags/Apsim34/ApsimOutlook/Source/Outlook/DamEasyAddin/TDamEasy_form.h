//---------------------------------------------------------------------------
#ifndef TSOI_formH
#define TSOI_formH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include <Buttons.hpp>
#include <ExtCtrls.hpp>
#include <vector>
#include "DEEconConfig.h"
//---------------------------------------------------------------------------
class TDamEasy_form : public TForm
{
__published:	// IDE-managed Components
   TLabel *Label1;
   TLabel *Label2;
   TLabel *Label3;
   TEdit *Sugar_price_box;
   TEdit *CCS_box;
   TPanel *Panel1;
   TLabel *Label6;
   TLabel *Label7;
   TEdit *Crop_cash_box;
   TLabel *Label8;
   TEdit *Overhead_box;
   TLabel *Label9;
   TEdit *Allocation_box;
   TLabel *Label10;
   TEdit *OOA_price_box;
   TLabel *Label11;
   TEdit *Storage_pumping_box;
   TLabel *Label12;
   TEdit *Irrigation_operating_box;
   TBitBtn *ApplyButton;
   TBitBtn *CancelButton;
   TLabel *Label13;
   TLabel *Label14;
   TLabel *Label16;
   TLabel *Label17;
   TLabel *Label18;
   TLabel *Label19;
   TLabel *Label20;
   TLabel *Label21;
   TLabel *Label22;
   TLabel *Label25;
   TLabel *Label26;
   TLabel *Label27;
   TLabel *Label34;
   TLabel *Label37;
   TEdit *Harvesting_and_levies_box;
   TEdit *Interest_rate_box;
   TEdit *Inflation_rate_input_box;
   TEdit *Inflation_rate_cane_box;
   TEdit *Repayment_time_box;
   TLabel *Label4;
   TLabel *Label41;
   TLabel *Label42;
   TLabel *Label46;
   TPanel *Panel3;
   TLabel *Label44;
   TLabel *Label48;
   TEdit *OFWS_pump_cost_box;
   TLabel *Label51;
   TRadioGroup *OFWS_payment_method;
   TPanel *Panel4;
   TLabel *Label5;
   TEdit *Number_partners_box;
   TComboBox *StartYearCombo;
   TLabel *Label29;
   TLabel *Label28;
   TLabel *Label30;
   TEdit *OFWS_construction_box;
   TLabel *Label31;
   TLabel *Label32;
   TEdit *Reticulation_box;
   TLabel *Label23;
   TLabel *Label24;
   TPanel *Panel2;
   TLabel *Label33;
   TEdit *Investment_rate_box;
   TLabel *Label35;
   TCheckBox *Tax_yes_no;
   TLabel *Label15;
   TEdit *Payment_constant_box;
   TComboBox *ConfigNameCombo;
   TLabel *RenameLabel;
   void __fastcall FormClose(TObject *Sender, TCloseAction &Action);
   void __fastcall FormShow(TObject *Sender);
   void __fastcall Repayment_time_boxExit(TObject *Sender);
   void __fastcall OFWS_payment_methodClick(TObject *Sender);
   void __fastcall Number_partners_boxExit(TObject *Sender);
   void __fastcall ConfigNameComboChange(TObject *Sender);
   void __fastcall ConfigNameComboKeyPress(TObject *Sender, char &Key);
   void __fastcall ConfigNameComboExit(TObject *Sender);
   void __fastcall RenameLabelClick(TObject *Sender);
private:	// User declarations
   std::vector<DEEconConfig>* Configs;
   DEEconConfig* currentConfig;
   bool editing;
   void PopulateForm(const DEEconConfig* Config);
   void StoreConfig();
   void createNewConfig(AnsiString name, DEEconConfig* config);

public:		// User declarations
   __fastcall TDamEasy_form(TComponent* Owner);
   __fastcall ~TDamEasy_form(void);
   void setMyConfigs(std::vector<DEEconConfig>& new_configs);
   int begin_year;
   int end_year;
};
//---------------------------------------------------------------------------
extern PACKAGE TDamEasy_form *DamEasy_form;
//---------------------------------------------------------------------------
#endif
