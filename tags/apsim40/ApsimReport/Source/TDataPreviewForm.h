//---------------------------------------------------------------------------

#ifndef TDataPreviewFormH
#define TDataPreviewFormH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include "AdvGrid.hpp"
#include "BaseGrid.hpp"
#include "DBAdvGrd.hpp"
#include <DB.hpp>
#include <Grids.hpp>
//---------------------------------------------------------------------------
class TDataPreviewForm : public TForm
{
__published:	// IDE-managed Components
   TDataSource *DataSource;
   TDBAdvStringGrid *DBAdvStringGrid1;
   void __fastcall DBAdvStringGrid1GetFloatFormat(TObject *Sender,
          int ACol, int ARow, bool &IsFloat, AnsiString &FloatFormat);
private:	// User declarations
public:		// User declarations
   __fastcall TDataPreviewForm(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TDataPreviewForm *DataPreviewForm;
//---------------------------------------------------------------------------
#endif
