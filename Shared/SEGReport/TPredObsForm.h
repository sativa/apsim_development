//---------------------------------------------------------------------------

#ifndef TPredObsFormH
#define TPredObsFormH
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
#include "TPredObs.h"
#include <DB.hpp>
#include "DBAdvGrd.hpp"
#include "AdvPanel.hpp"
#include <ExtCtrls.hpp>
//---------------------------------------------------------------------------
class TPredObsForm : public TPropertyForm
{
__published:	// IDE-managed Components
   TLabel *Label3;
   TComboBox *PredCombo;
   TLabel *Label1;
   TListBox *FieldNameList;
   TLabel *Label4;
   TComboBox *ObsCombo;
   void __fastcall PredComboChange(TObject *Sender);
   void __fastcall ObsComboChange(TObject *Sender);
   void __fastcall FieldNameListClick(TObject *Sender);
private:	// User declarations
   TPredObs* predobs;
   void SetupKeyFieldNameList();
public:		// User declarations
   __fastcall TPredObsForm(TComponent* Owner);
   void setComponent(TComponent* component);
};
//---------------------------------------------------------------------------
extern PACKAGE TPredObsForm *PredObsForm;
//---------------------------------------------------------------------------
#endif
