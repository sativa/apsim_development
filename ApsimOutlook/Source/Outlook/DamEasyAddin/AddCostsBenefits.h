//---------------------------------------------------------------------------

#ifndef AddCostsBenefitsH
#define AddCostsBenefitsH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include <ExtCtrls.hpp>
#include <Buttons.hpp>
#include <vector>
#include <string>

enum AnalysisT {NO_NPV_CALCS=0, ADDITIONAL_CALCS=1 , NPV_CALCS=2 };
typedef enum AnalysisT AnalysisType;


//---------------------------------------------------------------------------
class TAddCostsBenefitsForm : public TForm
{
__published:	// IDE-managed Components
   TRadioGroup *AnalysisTypeRadioGroup;
   TComboBox *BaseCaseComboBox;
   TLabel *Label1;
   TLabel *Label2;
   TEdit *InvestmentPeriodBox;
   TBitBtn *OKButton;
   TBitBtn *CancelButton;
   TLabel *Label3;
   TEdit *SalvageRateBox;
   TLabel *Label4;
   TLabel *Label5;
   void __fastcall AnalysisTypeRadioGroupClick(TObject *Sender);
   void __fastcall FormShow(TObject *Sender);
   void __fastcall FormClose(TObject *Sender, TCloseAction &Action);
   void __fastcall InvestmentPeriodBoxExit(TObject *Sender);
   void __fastcall FormCloseQuery(TObject *Sender, bool &CanClose);
public:		// User declarations
   __fastcall TAddCostsBenefitsForm(TComponent* Owner);
   void setScenarios(const std::vector<std::string>& current)
      {  scenario_names = current;   };
   AnalysisType getAnalysisType();
   std::string getBaseCase();
   int getInvestmentPeriod();
   double getSalvageRate();
   bool InvestmentPeriodBoxCheck();


private:	// User declarations
   void EnableControls(bool activate);
   std::vector<std::string> scenario_names;

   AnalysisType analysisType;
   std::string baseCase;
   int investmentPeriod;
   double salvageRate;




};
//---------------------------------------------------------------------------
extern PACKAGE TAddCostsBenefitsForm *AddCostsBenefitsForm;
//---------------------------------------------------------------------------
#endif
