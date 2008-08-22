//---------------------------------------------------------------------------

#ifndef TScenarioSelectFormH
#define TScenarioSelectFormH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
//---------------------------------------------------------------------------
class TScenarioSelectForm : public TForm
{
__published:	// IDE-managed Components
   TLabel *Label1;
   TComboBox *ScenarioList;
   TButton *Button1;
   TButton *Button2;
   void __fastcall FormShow(TObject *Sender);
private:	// User declarations
public:		// User declarations
   __fastcall TScenarioSelectForm(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TScenarioSelectForm *ScenarioSelectForm;
//---------------------------------------------------------------------------
#endif
