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
#include "TPropertyForm.h"
#include <Buttons.hpp>
#include <ComCtrls.hpp>
#include <Db.hpp>
#include <Dialogs.hpp>
#include <Grids.hpp>
#include "TApsimFileReader.h"
#include <DB.hpp>
#include "DBAdvGrd.hpp"
#include "AdvPanel.hpp"
#include <ExtCtrls.hpp>
//---------------------------------------------------------------------------
class TApsimFileReaderForm : public TPropertyForm
{
__published:	// IDE-managed Components
   TOpenDialog *OpenDialog;
   TLabel *Label3;
   TListView *FilesList;
   TCheckBox *InterpretCheckBox;
   TLabel *Label1;
   TLabel *Label4;
   void __fastcall InterpretCheckBoxClick(TObject *Sender);
   void __fastcall Label1Click(TObject *Sender);
   void __fastcall Label4Click(TObject *Sender);
private:	// User declarations
   TApsimFileReader* apsimFileReader;
public:		// User declarations
   __fastcall TApsimFileReaderForm(TComponent* Owner);
   virtual void setComponent(TComponent* component);

};
//---------------------------------------------------------------------------
extern PACKAGE TApsimFileReaderForm *ApsimFileReaderForm;
//---------------------------------------------------------------------------
#endif
