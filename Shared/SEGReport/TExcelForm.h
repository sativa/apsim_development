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
#include "TSEGTableForm.h"
#include <ComCtrls.hpp>
#include <Db.hpp>
#include <Grids.hpp>
#include "TExcel.h"
#include "AdvEdBtn.hpp"
#include "AdvEdit.hpp"
#include "AdvFileNameEdit.hpp"
#include <DB.hpp>
#include "DBAdvGrd.hpp"
//---------------------------------------------------------------------------
class TExcelForm : public TSEGTableForm
{
__published:	// IDE-managed Components
   TLabel *Label3;
   TAdvFileNameEdit *FilenameEdit;
   TLabel *Label4;
   TComboBox *PageCombo;
   void __fastcall FilenameEditChange(TObject *Sender);
   void __fastcall PageComboChange(TObject *Sender);
private:	// User declarations
   TExcel* excel;
public:		// User declarations
   __fastcall TExcelForm(TComponent* Owner);
   void setComponent(TExcel* excel);
};
//---------------------------------------------------------------------------
extern PACKAGE TExcelForm *ExcelForm;
//---------------------------------------------------------------------------
#endif
