//---------------------------------------------------------------------------
#ifndef TTabRenameFormH
#define TTabRenameFormH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include <Buttons.hpp>
//---------------------------------------------------------------------------
class TTabRenameForm : public TForm
{
__published:	// IDE-managed Components
   TLabel *Label1;
   TEdit *EditBox;
   TBitBtn *BitBtn1;
   TBitBtn *BitBtn2;
   void __fastcall FormShow(TObject *Sender);
   void __fastcall EditBoxChange(TObject *Sender);
private:	// User declarations
public:		// User declarations
   __fastcall TTabRenameForm(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TTabRenameForm *TabRenameForm;
//---------------------------------------------------------------------------
#endif
