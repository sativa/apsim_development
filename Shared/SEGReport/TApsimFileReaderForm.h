//---------------------------------------------------------------------------

#ifndef TApsimFileReaderFormH
#define TApsimFileReaderFormH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include "AdvGrid.hpp"
#include "BaseGrid.hpp"
#include "dbadvgrd.hpp"
#include "TSEGTableForm.h"
#include <Buttons.hpp>
#include <ComCtrls.hpp>
#include <Db.hpp>
#include <Dialogs.hpp>
#include <Grids.hpp>
#include "TApsimFileReader.h"
#include <DB.hpp>
#include "DBAdvGrd.hpp"
//---------------------------------------------------------------------------
class TApsimFileReaderForm : public TSEGTableForm
{
__published:	// IDE-managed Components
   TLabel *Label3;
   TListBox *FilesList;
   TSpeedButton *BrowseButton;
   TOpenDialog *OpenDialog;
   void __fastcall BrowseButtonClick(TObject *Sender);
private:	// User declarations
   TApsimFileReader* apsimFileReader;
public:		// User declarations
   __fastcall TApsimFileReaderForm(TComponent* Owner);
   void setComponent(TApsimFileReader* apsimFileReader);
};
//---------------------------------------------------------------------------
extern PACKAGE TApsimFileReaderForm *ApsimFileReaderForm;
//---------------------------------------------------------------------------
#endif
