//---------------------------------------------------------------------------

#ifndef TExcelFormH
#define TExcelFormH
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
#include "TExcel.h"
#include "AdvEdBtn.hpp"
#include "AdvEdit.hpp"
#include "AdvFileNameEdit.hpp"
#include <DB.hpp>
#include "DBAdvGrd.hpp"
#include "AdvPanel.hpp"
#include <ExtCtrls.hpp>
//---------------------------------------------------------------------------
class TExcelForm : public TPropertyForm
{
__published:	// IDE-managed Components
   TComboBox *PageCombo;
   TLabel *Label4;
   TAdvFileNameEdit *FilenameEdit;
   TLabel *Label3;
   void __fastcall FilenameEditChange(TObject *Sender);
   void __fastcall PageComboChange(TObject *Sender);
private:	// User declarations
   TExcel* excel;
public:		// User declarations
   __fastcall TExcelForm(TComponent* Owner);
   void setComponent(TComponent* comp);
};
//---------------------------------------------------------------------------
extern PACKAGE TExcelForm *ExcelForm;
//---------------------------------------------------------------------------
#endif
