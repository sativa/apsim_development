//---------------------------------------------------------------------------

#ifndef TRenameRotationFormH
#define TRenameRotationFormH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include <Buttons.hpp>
//---------------------------------------------------------------------------
class TRenameRotationForm : public TForm
{
__published:	// IDE-managed Components
   TEdit *RotationNameEdit;
   TLabel *Label1;
   TBitBtn *BitBtn1;
   TBitBtn *BitBtn2;
private:	// User declarations
public:		// User declarations
   __fastcall TRenameRotationForm(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TRenameRotationForm *RenameRotationForm;
//---------------------------------------------------------------------------
#endif
