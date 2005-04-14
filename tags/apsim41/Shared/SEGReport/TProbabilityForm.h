//---------------------------------------------------------------------------

#ifndef TProbabilityFormH
#define TProbabilityFormH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include "AdvGrid.hpp"
#include "BaseGrid.hpp"
#include "dbadvgrd.hpp"
#include "TPropertyForm.h"
#include <ComCtrls.hpp>
#include <Db.hpp>
#include <Grids.hpp>
#include "TProbability.h"
#include <DB.hpp>
#include "AdvPanel.hpp"
#include <ExtCtrls.hpp>
//---------------------------------------------------------------------------
class TProbabilityForm : public TPropertyForm
{
__published:	// IDE-managed Components
   TComboBox *FieldNameCombo;
   TCheckBox *ExceedenceCheckBox;
   TLabel *Label3;
   void __fastcall FieldNameComboChange(TObject *Sender);
   void __fastcall ExceedenceCheckBoxClick(TObject *Sender);
private:	// User declarations
   TProbability* probability;
public:		// User declarations
   __fastcall TProbabilityForm(TComponent* Owner);
   virtual void setComponent(TComponent* component);
};
//---------------------------------------------------------------------------
extern PACKAGE TProbabilityForm *ProbabilityForm;
//---------------------------------------------------------------------------
#endif
