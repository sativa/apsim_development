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
#include "AdvPanel.hpp"
#include <ExtCtrls.hpp>
//---------------------------------------------------------------------------
class TREMSForm : public TPropertyForm
{
__published:	// IDE-managed Components
   TComboBox *DataSourceCombo;
   TComboBox *TreatmentCombo;
   TComboBox *ExperimentCombo;
   TAdvFileNameEdit *FilenameEdit;
   TLabel *Label3;
   TLabel *Label4;
   TLabel *Label5;
   TLabel *Label6;
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
