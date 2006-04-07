//---------------------------------------------------------------------------

#ifndef TXmlFileReaderFormH
#define TXmlFileReaderFormH
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
#include "TXmlFileReader.h"
#include <DB.hpp>
#include "DBAdvGrd.hpp"
#include "AdvPanel.hpp"
#include <ExtCtrls.hpp>
//---------------------------------------------------------------------------
class TXmlFileReaderForm : public TPropertyForm
{
__published:	// IDE-managed Components
   TOpenDialog *OpenDialog;
   TLabel *Label3;
   TLabel *Label1;
   TEdit *FileNameEdit;
   void __fastcall Label1Click(TObject *Sender);
private:	// User declarations
   TXmlFileReader* xmlFileReader;
public:		// User declarations
   __fastcall TXmlFileReaderForm(TComponent* Owner);
   virtual void setComponent(TComponent* component);

};
//---------------------------------------------------------------------------
extern PACKAGE TXmlFileReaderForm *XmlFileReaderForm;
//---------------------------------------------------------------------------
#endif
