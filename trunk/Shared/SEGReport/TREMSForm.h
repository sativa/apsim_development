//---------------------------------------------------------------------------

#ifndef TREMSFormH
#define TREMSFormH
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
#include "TREMS.h"
#include "AdvEdBtn.hpp"
#include "AdvEdit.hpp"
#include "AdvFileNameEdit.hpp"
#include <DB.hpp>
//---------------------------------------------------------------------------
class TREMSForm : public TPropertyForm
{
__published:	// IDE-managed Components
   TLabel *Label3;
   TAdvFileNameEdit *FilenameEdit;
   TLabel *Label4;
   TComboBox *ExperimentCombo;
   TLabel *Label5;
   TComboBox *TreatmentCombo;
   TLabel *Label6;
   TComboBox *DataSourceCombo;
   void __fastcall FilenameEditChange(TObject *Sender);
   void __fastcall ExperimentComboChange(TObject *Sender);
   void __fastcall TreatmentComboChange(TObject *Sender);
   void __fastcall DataSourceComboChange(TObject *Sender);
private:	// User declarations
   TREMS* rems;
public:		// User declarations
   __fastcall TREMSForm(TComponent* Owner);
   virtual void setComponent(TComponent* rems);
};
//---------------------------------------------------------------------------
extern PACKAGE TREMSForm *REMSForm;
//---------------------------------------------------------------------------
#endif
