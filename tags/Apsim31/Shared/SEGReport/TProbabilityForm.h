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
#include "TSEGTableForm.h"
#include <ComCtrls.hpp>
#include <Db.hpp>
#include <Grids.hpp>
#include "TProbability.h"
#include <DB.hpp>
//---------------------------------------------------------------------------
class TProbabilityForm : public TSEGTableForm
{
__published:	// IDE-managed Components
   TGroupBox *GroupBox1;
   TRadioButton *ExceedenceRadio;
   TRadioButton *CumulativeRadio;
   TLabel *Label3;
   TComboBox *FieldNameCombo;
   void __fastcall ExceedenceRadioClick(TObject *Sender);
   void __fastcall CumulativeRadioClick(TObject *Sender);
   void __fastcall PropertiesSheetShow(TObject *Sender);
   void __fastcall FieldNameComboChange(TObject *Sender);
private:	// User declarations
   TProbability* probability;
public:		// User declarations
   __fastcall TProbabilityForm(TComponent* Owner);
   void setComponent(TProbability* probability);
};
//---------------------------------------------------------------------------
extern PACKAGE TProbabilityForm *ProbabilityForm;
//---------------------------------------------------------------------------
#endif
